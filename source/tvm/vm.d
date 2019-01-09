module tvm.vm;
import tvm.parser, tvm.value, tvm.util, tvm.opcode;
import std.algorithm, std.format, std.conv;
import std.stdio;

class Env {
  IValue[string] variables;

  void addVMFunction(string func_name, Opcode[] func_body) {
    this.variables[func_name] = new VMFunction(func_name, func_body, this.dup);
  }

  Env dup() {
    Env newEnv = new Env;
    return newEnv;
  }
}

class VM {
  Env env;
  Stack!IValue stack;

  this() {
    this.env = new Env;
    this.stack = new Stack!IValue;
    // dfmt off
    this.env.variables["sq"] = new IValue(new VMFunction("sq", [opSetVariablePop, new IValue("n"),
                                                                opGetVariable, new IValue("n"),
                                                                opGetVariable, new IValue("n"), opMul], env.dup));
    // dfmt on
    this.env.variables["print"] = new IValue(new VMFunction("print", [opPrint], env));
    this.env.variables["println"] = new IValue(new VMFunction("println", [opPrintln], env));
  }

  IValue execute(Opcode[] code) {
    IValue stackPeekTop() {
      if (!stack.empty) {
        return stack.front;
      } else {
        return null;
      }
    }

    for (size_t pc; pc < code.length; pc++) {
      Opcode op = code[pc];
      //writeln("op : ", op.type);
      final switch (op.type) with (OpcodeType) {
      case tOpVariableDeclareOnlySymbol:
        auto symbol = cast(IValue)code[pc++ + 1];
        this.env.variables[symbol.getString] = new IValue;
        break;
      case tOpVariableDeclareWithAssign:
        auto symbol = cast(IValue)code[pc++ + 1];
        auto v = stack.pop;
        this.env.variables[symbol.getString] = v;
        break;
      case tOpAssignExpression:
        auto symbol = cast(IValue)code[pc++ + 1];
        auto v = stack.pop;
        this.env.variables[symbol.getString] = v;
        break;
      case tOpPush:
        auto v = cast(IValue)code[pc++ + 1];
        assert(v !is null, "Execute Error on tOpPush");
        stack.push(v);
        break;
      case tOpPop:
        stack.pop;
        break;
      case tOpAdd:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a + b);
        break;
      case tOpSub:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a - b);
        break;
      case tOpMul:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a * b);
        break;
      case tOpDiv:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a / b);
        break;
      case tOpMod:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a % b);
        break;
      case tOpReturn:
        return stackPeekTop;
      case tOpGetVariable:
        auto v = cast(IValue)code[pc++ + 1];
        assert(v !is null, "Execute Error on tOpGetVariable");
        if (v.getString in env.variables) {
          stack.push(env.variables[v.getString]);
        } else {
          throw new Exception("No such a variable %s".format(v.getString));
        }
        break;
      case tOpSetVariablePop:
        auto dst = cast(IValue)code[pc++ + 1];
        auto v = stack.pop;
        this.env.variables[dst.getString] = v;
        break;
      case tOpCall:
        auto func = cast(IValue)code[pc++ + 1];
        string fname = func.getString;
        Env cpyEnv = this.env;
        this.env = this.env.variables[fname].getFunction.func_env.dup;
        this.execute(cpyEnv.variables[fname].getFunction.func_body);
        this.env = cpyEnv;
        break;
      case tOpNop:
        break;
      case tOpFunctionDeclare:
        auto symbol = cast(IValue)code[pc++ + 1];
        string func_name = symbol.getString;
        auto op_blocks_length = cast(IValue)code[pc++ + 1];
        Opcode[] func_body;
        foreach (_; 0 .. op_blocks_length.getLong) {
          func_body ~= code[pc++ + 1];
        }
        this.env.variables[func_name] = new IValue(new VMFunction(func_name, func_body, env.dup));
        break;
      case tOpEqualExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a == b));
        break;
      case tOpNotEqualExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a != b));
        break;
      case tOpLtExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a < b));
        break;
      case tOpLteExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a <= b));
        break;
      case tOpGtExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a > b));
        break;
      case tOpGteExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a >= b));
        break;
      case tOpAndExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a && b));
        break;
      case tOpOrExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a || b));
        break;
      case tOpXorExpression:
        throw new Error("Not implemented <%s>".format(op.type));
      case tOpPrint:
        auto v = stack.pop;
        if (v.vtype == ValueType.String) {
          write(v.getString);
        } else {
          write(v);
        }
        break;
      case tOpPrintln:
        auto v = stack.pop;
        if (v.vtype == ValueType.String) {
          writeln(v.getString);
        } else {
          writeln(v);
        }
        break;
      case tOpJumpRel:
        auto v = cast(IValue)code[pc++ + 1];
        pc += v.getLong;
        break;
      case tOpJumpAbs:
        auto v = cast(IValue)code[pc++ + 1];
        pc = v.getLong;
        break;
      case tOpIFStatement:
        auto cond = stack.pop;
        bool condResult;

        final switch (cond.vtype) with (ValueType) {
        case Long:
          condResult = cond.getLong != 0;
          break;
        case Bool:
          condResult = cond.getBool;
          break;
        case String:
          throw new Exception("Execute Error Invalid Condition <string>");
        case Array:
          throw new Exception("Execute Error Invalid Condition <array>");
        case Function:
          throw new Exception("Execute Error Invalid Condition <function>");
        case Null:
          condResult = false;
          break;
        }

        auto trueBlockLength = (cast(IValue)code[pc++ + 1]).getLong;

        if (condResult) {
          break;
        } else {
          pc += trueBlockLength;
        }
        break;
      case tOpSetArrayElement:
        auto variable = (cast(IValue)code[pc++ + 1]).getString;
        auto idx = (cast(IValue)code[pc++ + 1]).getLong;
        auto val = stack.pop;
        env.variables[variable].setArrayElement(idx, val);
        break;
      case tOpGetArrayElement:
        auto variable = (cast(IValue)code[pc++ + 1]).getString;
        auto idx = (cast(IValue)code[pc++ + 1]).getLong;
        stack.push(env.variables[variable][idx]);
        break;
      case tIValue:
        throw new Error("IValue should not peek directly");
      }
    }
    return stackPeekTop();
  }
}
