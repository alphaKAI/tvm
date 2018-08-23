module tvm.vm;
import tvm.parser, tvm.value, tvm.util;
import std.algorithm, std.format, std.conv;
import std.stdio;

enum RegisterID {
  A,
  B,
  C,
  D,
  E,
  F,
  RET
}

class Registers {
  IValue A, B, C, D, E, F, RET;

  void setRegister(RegisterID id, IValue value) {
    final switch (id) with (RegisterID) {
    case A:
      this.A = value;
      break;
    case B:
      this.B = value;
      break;
    case C:
      this.C = value;
      break;
    case D:
      this.D = value;
      break;
    case E:
      this.E = value;
      break;
    case F:
      this.F = value;
      break;
    case RET:
      this.RET = value;
      break;
    }
  }

  IValue getRegister(RegisterID id) {
    final switch (id) with (RegisterID) {
    case A:
      return this.A;
    case B:
      return this.B;
    case C:
      return this.C;
    case D:
      return this.D;
    case E:
      return this.E;
    case F:
      return this.F;
    case RET:
      return this.RET;
    }
  }

  static RegisterID getRegisterID(string reg) {
    switch (reg) with (RegisterID) {
    case "A":
      return RegisterID.A;
    case "B":
      return RegisterID.B;
    case "C":
      return RegisterID.C;
    case "D":
      return RegisterID.D;
    case "E":
      return RegisterID.E;
    case "F":
      return RegisterID.F;
    case "RET":
      return RegisterID.RET;
    default:
      throw new Error("<Register Access Error>No such a register - %s".format(reg));
    }
  }
}

enum OpcodeType {
  tOpVariableDeclareOnlySymbol,
  tOpVariableDeclareWithAssign,
  tOpPop,
  tOpPush,
  tOpAdd,
  tOpSub,
  tOpMul,
  tOpDiv,
  tOpMod,
  tOpReturn,
  tOpGetVariable,
  tOpSetVariableR,
  tOpSetVariableI,
  tOpSetVariablePop,
  tOpMovR,
  tOpMovI,
  tOpCall,
  tOpPushR,
  tOpPopR,
  tOpNop,
  tOpFunctionDeclare,
  tOpEqualExpression,
  tOpNotEqualExpression,
  tOpLtExpression,
  tOpLteExpression,
  tOpGtExpression,
  tOpGteExpression,
  tOpAndExpression,
  tOpOrExpression,
  tOpXorExpression,
  tOpJumpRel,
  tOpJumpAbs,
  tOpPrint,
  tOpPrintln,
  tOpIFStatement,
  tIValue
}

interface Opcode {
  OpcodeType type();
}

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
    /*
    foreach (name, value; this.variables) {
      newEnv.variables[name] = value.dup;
    }*/

    return newEnv;
  }
}

string genTypeMethod(T)() {
  return q{
    OpcodeType type() {
      return OpcodeType.t%s;
    }
  }.format(T.stringof);
}

string genOpCodeClassAndHelper(string t)() {
  string base = t[1 .. $];
  if (base == "IValue") {
    return "";
  }

  string helper = "o" ~ base[1 .. $];

  return q{
class %s : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode %s() {
  return new %s;
}
}.format(base, helper, base);
}

static foreach (elem; __traits(allMembers, OpcodeType)) {
  mixin(genOpCodeClassAndHelper!(elem));
}

Opcode[] compileASTtoOpcode(AST ast) {
  final switch (ast.type) with (ASTType) {
  case tIdentifier:
    auto ident = cast(Identifier)ast;
    assert(ident !is null, "Compile Error on <%s>".format(ast.type));
    return [new IValue(ident.value)];
  case tSymbol:
    auto symbol = cast(Symbol)ast;
    assert(symbol !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(symbol.ident);
  case tParens:
    auto parens = cast(Parens)ast;
    assert(parens !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(parens.expression);
  case tVariable:
    auto var = cast(Variable)ast;
    assert(var !is null, "Compile Error on <%s>".format(ast.type));
    return [opGetVariable] ~ compileASTtoOpcode(var.ident);
  case tStringLiteral:
    auto value = cast(StringLiteral)ast;
    return [new IValue(value.value)];
  case tInteger:
    auto value = cast(Integer)ast;
    return [new IValue(value.value)];
  case tBooleanLiteral:
    auto value = cast(BooleanLiteral)ast;
    return [new IValue(value.value)];
  case tParameter:
    auto param = cast(Parameter)ast;
    assert(param !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(param.expr);
  case tParameterList:
    Opcode[] ret;
    auto params = cast(ParameterList)ast;
    assert(params !is null, "Compile Error on <%s>".format(ast.type));
    foreach (parameter; params.parameters) {
      ret ~= compileASTtoOpcode(parameter);
    }
    return ret;
  case tStatementList:
    Opcode[] ret;
    auto slist = cast(StatementList)ast;
    assert(slist !is null, "Compile Error on <%s>".format(ast.type));
    foreach (statement; slist.statements) {
      ret ~= compileASTtoOpcode(statement);
    }

    return ret;
  case tBlock:
    auto block = cast(Block)ast;
    assert(block !is null, "Compile Error on <%s>".format(ast.type));
    auto statements = block.statements;
    if (statements is null) {
      return [opNop];
    } else {
      return compileASTtoOpcode(block.statements);
    }
  case tIFStatement:
    auto ifStmt = cast(IFStatement)ast;
    Expression cond = ifStmt.cond;
    Block trueBlock = ifStmt.trueBlock, falseBlock = ifStmt.falseBlock;
    Opcode[] op_trueBlock = compileASTtoOpcode(trueBlock), op_falseBlock;
    if (falseBlock !is null) {
      op_falseBlock = compileASTtoOpcode(falseBlock);
      long falseBlockLength = op_falseBlock.length.to!long;
      op_trueBlock ~= [opJumpRel, new IValue(falseBlockLength)];
    }
    long trueBlockLength = op_trueBlock.length.to!long;

    return compileASTtoOpcode(cond) ~ [opIFStatement] ~ cast(
        Opcode[])[new IValue(trueBlockLength)] ~ op_trueBlock ~ op_falseBlock;
  case tForStatement:
    /*
    auto forStmt = casT(ForStatement) ast;
    assert(forStmt !is null, "Compile Error on <%s>".format(ast.type));
    VariableDeclare vassign = forStmt.vassign;
    Expression cond = forStmt.cond, update = forStmt.update;
    Block block = forStmt.block;
    */
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tFunctionDeclare:
    auto func = cast(FunctionDeclare)ast;
    assert(func !is null, "Compile Error on <%s>".format(ast.type));
    Symbol symbol = func.symbol;
    ParameterList parameters = func.parameters;
    Block block = func.block;

    Opcode[] op_params;
    foreach (elem; compileASTtoOpcode(parameters)) {
      if (elem.type != OpcodeType.tOpGetVariable) {
        op_params ~= elem;
      }
    }
    long op_params_count = op_params.length.to!long;

    Opcode[] op_blocks = compileASTtoOpcode(block);
    Opcode[] prepends;
    foreach_reverse (op_param; op_params) {
      auto param = cast(IValue)op_param;
      assert(param !is null, "Compile Error on <%s> [param]".format(ast.type));
      prepends ~= [opSetVariablePop, param];
    }

    op_blocks = prepends ~ op_blocks;

    long op_blocks_count = op_blocks.length.to!long;

    auto ret = [opFunctionDeclare] ~ compileASTtoOpcode(symbol) ~ cast(
        Opcode[])[new IValue(op_params_count)] ~ op_params ~ cast(
        Opcode[])[new IValue(op_blocks_count)] ~ op_blocks;
    return ret;
  case tVariableDeclareOnlySymbol:
    auto var = cast(VariableDeclareOnlySymbol)ast;
    assert(var !is null, "Compile Error on <%s>".format(ast.type));
    auto l = compileASTtoOpcode(var.lvalue);
    if (l.length == 2 && l[0].type == OpcodeType.tOpGetVariable) {
      l = l[1 .. $];
    }
    return [opVariableDeclareOnlySymbol] ~ l;
  case tVariableDeclareWithAssign:
    auto var = cast(VariableDeclareWithAssign)ast;
    assert(var !is null, "Compile Error on <%s>".format(ast.type));
    auto l = compileASTtoOpcode(var.lvalue);
    if (l.length == 2 && l[0].type == OpcodeType.tOpGetVariable) {
      l = l[1 .. $];
    }
    auto e = compileASTtoOpcode(var.expr);
    if (e.length == 2 && e[0].type == OpcodeType.tOpGetVariable) {
      e = e[1 .. $];
    }
    return e ~ [opVariableDeclareWithAssign] ~ l;
  case tAssignExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tAddExpression:
    auto expr = cast(AddExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opAdd];
  case tSubExpression:
    auto expr = cast(SubExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opSub];
  case tMulExpression:
    auto expr = cast(MulExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opMul];
  case tDivExpression:
    auto expr = cast(DivExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opDiv];
  case tModExpression:
    auto expr = cast(ModExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opMod];
  case tCallExpression:
    auto call = cast(CallExpression)ast;
    assert(call !is null, "Compile Error on <%s>".format(ast.type));
    Symbol symbol = call.symbol;
    ParameterList parameters = call.parameters;
    return compileASTtoOpcode(parameters) ~ opCall ~ compileASTtoOpcode(symbol);
  case tReturnExpression:
    auto ret = cast(ReturnExpression)ast;
    assert(ret !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(ret.expression) ~ [opReturn];
  case tEqualExpression:
    auto expr = cast(EqualExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opEqualExpression];
  case tNotEqualExpression:
    auto expr = cast(NotEqualExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opNotEqualExpression];
  case tLtExpression:
    auto expr = cast(LtExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opLtExpression];
  case tLteExpression:
    auto expr = cast(LteExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opLteExpression];
  case tGtExpression:
    auto expr = cast(GtExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opGtExpression];
  case tGteExpression:
    auto expr = cast(GteExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opGteExpression];
  case tAndExpression:
    auto expr = cast(AndExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opAndExpression];
  case tOrExpression:
    auto expr = cast(OrExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opOrExpression];
  case tXorExpression:
    auto expr = cast(XorExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    if (r.length == 1 && (cast(IValue)r[0]) !is null) {
      r = [opPush] ~ r;
    }
    if (l.length == 1 && (cast(IValue)l[0]) !is null) {
      l = [opPush] ~ l;
    }
    return r ~ l ~ [opXorExpression];
  }
}

class VM {
  Env env;
  Registers registers;
  Stack!IValue stack = new Stack!IValue;

  this() {
    this.env = new Env;
    this.registers = new Registers;

    this.env.variables["a"] = new IValue(100);
    this.env.funcs["f"] = new FuncScope("f", [opMovI, new IValue("RET"),
        new IValue("100")], env.dup);
    this.env.funcs["sq"] = new FuncScope("sq", [opPopR, new IValue("A"), opPushR,
        new IValue("A"), opPushR, new IValue("A"), opMul], env.dup);
    this.env.funcs["print"] = new FuncScope("print", [opPrint], env);
    this.env.funcs["println"] = new FuncScope("println", [opPrintln], env);
  }

  IValue execute(Opcode[] code) {
    for (size_t pc; pc < code.length; pc++) {
      Opcode op = code[pc];
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
        registers.RET = a + b;
        stack.push(registers.RET);
        break;
      case tOpSub:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = a - b;
        stack.push(registers.RET);
        break;
      case tOpMul:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = a * b;
        stack.push(registers.RET);
        break;
      case tOpDiv:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = a / b;
        stack.push(registers.RET);
        break;
      case tOpMod:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = a % b;
        stack.push(registers.RET);
        break;
      case tOpReturn:
        return registers.RET;
      case tOpGetVariable:
        auto v = cast(IValue)code[pc++ + 1];
        assert(v !is null, "Execute Error on tOpGetVariable");
        stack.push(env.variables[v.getString]);
        break;
      case tOpSetVariableR:
        auto dst = cast(IValue)code[pc++ + 1];
        auto src = cast(IValue)code[pc++ + 1];
        this.env.variables[dst.getString] = registers.getRegister(
            Registers.getRegisterID(src.getString));
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
      case tOpMovR:
        auto dst = cast(IValue)code[pc++ + 1];
        auto src = cast(IValue)code[pc++ + 1];
        registers.setRegister(Registers.getRegisterID(dst.getString),
            registers.getRegister(Registers.getRegisterID(src.getString)));
        break;
      case tOpMovI:
        auto dst = cast(IValue)code[pc++ + 1];
        auto v = cast(IValue)code[pc++ + 1];
        registers.setRegister(Registers.getRegisterID(dst.getString), v);
        break;
      case tOpCall:
        auto func = cast(IValue)code[pc++ + 1];
        string fname = func.getString;
        Env cpyEnv = this.env;
        this.env = this.env.funcs[fname].func_env;
        registers.RET = this.execute(cpyEnv.funcs[fname].func_body);
        this.env = cpyEnv;
        break;
      case tOpPushR:
        auto dst = cast(IValue)code[pc++ + 1];
        stack.push(registers.getRegister(Registers.getRegisterID(dst.getString)));
        break;
      case tOpPopR:
        auto dst = cast(IValue)code[pc++ + 1];
        registers.setRegister(Registers.getRegisterID(dst.getString), stack.pop());
        break;
      case tOpNop:
        break;
      case tOpFunctionDeclare:
        auto symbol = cast(IValue)code[pc++ + 1];
        string func_name = symbol.getString;
        auto params_count = cast(IValue)code[pc++ + 1];
        string[] params;
        foreach (_; 0 .. params_count.getLong) {
          params ~= (cast(IValue)code[pc++ + 1]).getString;
        }
        auto op_count = cast(IValue)code[pc++ + 1];
        Opcode[] func_body;
        foreach (_; 0 .. op_count.getLong) {
          func_body ~= code[pc++ + 1];
        }
        this.env.funcs[func_name] = new FuncScope(func_name, func_body, env.dup);
        break;
      case tOpEqualExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a == b);
        stack.push(registers.RET);
        break;
      case tOpNotEqualExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a != b);
        stack.push(registers.RET);
        break;
      case tOpLtExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a < b);
        stack.push(registers.RET);
        break;
      case tOpLteExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a <= b);
        stack.push(registers.RET);
        break;
      case tOpGtExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a > b);
        stack.push(registers.RET);
        break;
      case tOpGteExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a >= b);
        stack.push(registers.RET);
        break;
      case tOpAndExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a == b);
        stack.push(registers.RET);
        break;
      case tOpOrExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a == b);
        stack.push(registers.RET);
        break;
      case tOpXorExpression:
        IValue a = stack.pop, b = stack.pop;
        registers.RET = new IValue(a == b);
        stack.push(registers.RET);
        break;
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
        stack.push(cast(IValue)op);
        break;
      }
    }

    return registers.RET;
  }
}

/*
  VariableDeclare < VariableDeclareWithAssign / VariableDeclareOnlySymbol
  VariableDeclareOnlySymbol < "var" LeftValue
  VariableDeclareWithAssign < "var" LeftValue "=" Expression

  ParameterList < "()" / :"(" Parameter ("," Parameter)* :")"
  Parameter < Expression

  Value < LeftValue / RightValue
  LeftValue < Variable
  Variable < Identifier
  RightValue < Integer / StringLiteral / BooleanLiteral
  AssignExpression < LeftValue "=" Expression
  ReturnExpression < "return" Expression
  CallExpression < Symbol ParameterList
  
  CompareExpression < EqualExpression / LtExpression / LteExpression / GtExpression / GteExpression
  EqualExpression < Expression "==" Expression
  LtExpression < Expression "<" Expression
  LteExpression < Expression "<=" Expression
  GtExpression < Expression ">" Expression
  GteExpression < Expression ">=" Expression

  LogicExpression < AndExpression / OrExpression / XorExpression
  AndExpression < Expression "&&" Expression
  OrExpression < Expression "||" Expression
  XorExpression < Expression "^" Expression

  Expression < CompareExpression / LogicExpression / AssignExpression / MathExpression / Parens / CallExpression / ReturnExpression / Value
  Parens < :"(" Expression :")"

  IFStatement < "if" :"(" Expression :")" Block ("else" Block)?

  Statement < FunctionDeclare / IFStatement / ((VariableDeclare / Expression) ";")
  StatementList < Statement+

  Block < "{" StatementList? "}"

  BooleanLiteral < "true" / "false"
*/
