module tvm.vm;
import tvm.parser, tvm.value, tvm.util, tvm.opcode;
import std.algorithm, std.format;
import std.stdio;

class FuncScope {
  string func_name;
  Opcode[] func_body;
  Env func_env;

  this(string func_name, Opcode[] func_body, Env func_env) {
    this.func_name = func_name;
    this.func_body = func_body;
    this.func_env = func_env;
  }
}

class Env {
  FuncScope[string] funcs;
  IValue[string] variables;

  void addFuncScope(string func_name, Opcode[] func_body) {
    funcs[func_name] = new FuncScope(func_name, func_body, this.dup);
  }

  Env dup() {
    Env newEnv = new Env;
    newEnv.funcs = this.funcs;

    return newEnv;
  }
}

class VM {
  Env env;
  Stack!IValue stack;

  this() {
    this.env = new Env;
    this.stack = new Stack!IValue;

    this.env.funcs["sq"] = new FuncScope("sq", [opSetVariablePop, new IValue("n"),
        opGetVariable, new IValue("n"), opGetVariable, new IValue("n"), opMul], env.dup);
    this.env.funcs["print"] = new FuncScope("print", [opPrint], env);
    this.env.funcs["println"] = new FuncScope("println", [opPrintln], env);
  }

  IValue execute(Opcode[] code) {
    IValue stackPeekTop() {
      if (stack.stack.length) {
        return stack.front;
      } else {
        return null;
      }
    }

    for (size_t pc; pc < code.length; pc++) {
      Opcode op = code[pc];
      //writeln("stack : ", stack.stack);
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
        a.addTo(b);
        stack.push(a);
        break;
      case tOpSub:
        IValue a = stack.pop, b = stack.pop;
        a.subTo(b);
        stack.push(a);
        break;
      case tOpMul:
        IValue a = stack.pop, b = stack.pop;
        a.mulTo(b);
        stack.push(a);
        break;
      case tOpDiv:
        IValue a = stack.pop, b = stack.pop;
        a.divTo(b);
        stack.push(a);
        break;
      case tOpMod:
        IValue a = stack.pop, b = stack.pop;
        a.modTo(b);
        stack.push(a);
        break;
      case tOpReturn:
        return stackPeekTop;
      case tOpGetVariable:
        auto v = cast(IValue)code[pc++ + 1];
        assert(v !is null, "Execute Error on tOpGetVariable");
        stack.push(env.variables[v.getString]);
        break;
      case tOpSetVariableI:
        auto dst = cast(IValue)code[pc++ + 1];
        auto v = cast(IValue)code[pc++ + 1];
        this.env.variables[dst.getString] = v;
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
        this.env = this.env.funcs[fname].func_env.dup;
        this.execute(cpyEnv.funcs[fname].func_body);
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
        this.env.funcs[func_name] = new FuncScope(func_name, func_body, env.dup);
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
      case tIValue:
        throw new Error("IValue should not peek directly");
      }
    }
    return stackPeekTop();
  }
}
