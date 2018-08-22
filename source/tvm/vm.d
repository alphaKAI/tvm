module tvm.vm;
import tvm.parser, tvm.value, tvm.util;
import std.algorithm, std.format;
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
  tOpMovR,
  tOpMovI,
  tOpCall,
  tOpPushR,
  tOpPopR,
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
    foreach (name, value; this.variables) {
      newEnv.variables[name] = value.dup;
    }

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

class OpVariableDeclareOnlySymbol : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opVariableDeclareOnlySymbol() {
  return new OpVariableDeclareOnlySymbol;
}

class OpVariableDeclareWithAssign : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opVariableDeclareWithAssign() {
  return new OpVariableDeclareWithAssign;
}

class OpPush : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opPush() {
  return new OpPush;
}

class OpPop : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opPop() {
  return new OpPop();
}

interface Function : Opcode {
  void perform(Registers registers);
}

class OpAdd : Function {
  mixin(genTypeMethod!(typeof(this)));

  void perform(Registers registers) {
    IValue l = cast(IValue)registers.A;
    assert(l !is null, "Execute Error on OpAdd#perform<l>");
    IValue r = cast(IValue)registers.B;
    assert(r !is null, "Execute Error on OpAdd#perform<r>");
    registers.setRegister(RegisterID.RET, l + r);
  }
}

Opcode opAdd() {
  return new OpAdd();
}

class OpSub : Function {
  mixin(genTypeMethod!(typeof(this)));

  void perform(Registers registers) {
    IValue l = cast(IValue)registers.A;
    assert(l !is null, "Execute Error on OpSub#perform<l>");
    IValue r = cast(IValue)registers.B;
    assert(r !is null, "Execute Error on OpSub#perform<r>");
    registers.setRegister(RegisterID.RET, l - r);
  }
}

Opcode opSub() {
  return new OpSub();
}

class OpMul : Function {
  mixin(genTypeMethod!(typeof(this)));

  void perform(Registers registers) {
    IValue l = cast(IValue)registers.A;
    assert(l !is null, "Execute Error on OpMul#perform<l>");
    IValue r = cast(IValue)registers.B;
    assert(r !is null, "Execute Error on OpMul#perform<r>");
    registers.setRegister(RegisterID.RET, l * r);
  }
}

Opcode opMul() {
  return new OpMul();
}

class OpDiv : Function {
  mixin(genTypeMethod!(typeof(this)));

  void perform(Registers registers) {
    IValue l = cast(IValue)registers.A;
    assert(l !is null, "Execute Error on OpDiv#perform<l>");
    IValue r = cast(IValue)registers.B;
    assert(r !is null, "Execute Error on OpDiv#perform<r>");
    registers.setRegister(RegisterID.RET, l / r);
  }
}

Opcode opDiv() {
  return new OpDiv();
}

class OpMod : Function {
  mixin(genTypeMethod!(typeof(this)));

  void perform(Registers registers) {
    IValue l = cast(IValue)registers.A;
    assert(l !is null, "Execute Error on OpMod#perform<l>");
    IValue r = cast(IValue)registers.B;
    assert(r !is null, "Execute Error on OpMod#perform<r>");
    registers.setRegister(RegisterID.RET, l % r);
  }
}

Opcode opMod() {
  return new OpMod();
}

class OpReturn : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opReturn() {
  return new OpReturn;
}

class OpGetVariable : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opGetVariable() {
  return new OpGetVariable;
}

class OpMovR : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opMovR() {
  return new OpMovR;
}

class OpMovI : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opMovI() {
  return new OpMovI;
}

class OpCall : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opCall() {
  return new OpCall;
}

class OpPushR : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opPushR() {
  return new OpPushR;
}

class OpPopR : Opcode {
  mixin(genTypeMethod!(typeof(this)));
}

Opcode opPopR() {
  return new OpPopR;
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
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tInteger:
    auto value = cast(Integer)ast;
    return [new IValue(value.value)];
  case tBooleanLiteral:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tParameter:
    auto param = cast(Parameter)ast;
    assert(param !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(param.expr);
  case tParameterList:
    Opcode[] ret;
    auto params = cast(ParameterList)ast;
    assert(params !is null, "Compile Error on <%s>".format(ast.type));
    string[] regs = ["A", "B", "C", "D", "E", "F"];
    foreach (i, parameter; params.parameters) {
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
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tIFStatement:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tFunctionDeclare:
    throw new Error("Not Implemented <%s>".format(ast.type));
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
    return [opVariableDeclareWithAssign] ~ l ~ e;
  case tAssignExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tAddExpression:
    auto expr = cast(AddExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    //return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opAdd];
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
    //return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opSub];
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
    //return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opMul];
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
    //return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opDiv];
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
    //return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opMod];
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
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tLtExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tLteExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tGtExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tGteExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tAndExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tOrExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tXorExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
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
        auto v = cast(IValue)code[pc++ + 1];
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
        auto add = cast(OpAdd)op;
        registers.A = stack.pop;
        registers.B = stack.pop;
        add.perform(registers);
        stack.push(registers.RET);
        break;
      case tOpSub:
        auto sub = cast(OpSub)op;
        registers.A = stack.pop;
        registers.B = stack.pop;
        sub.perform(registers);
        stack.push(registers.RET);
        break;
      case tOpMul:
        auto mul = cast(OpMul)op;
        registers.A = stack.pop;
        registers.B = stack.pop;
        mul.perform(registers);
        stack.push(registers.RET);
        break;
      case tOpDiv:
        auto div = cast(OpDiv)op;
        registers.A = stack.pop;
        registers.B = stack.pop;
        div.perform(registers);
        stack.push(registers.RET);
        break;
      case tOpMod:
        auto mod = cast(OpMod)op;
        registers.A = stack.pop;
        registers.B = stack.pop;
        mod.perform(registers);
        stack.push(registers.RET);
        break;
      case tOpReturn:
        return registers.RET;
      case tOpGetVariable:
        auto v = cast(IValue)code[pc++ + 1];
        assert(v !is null, "Execute Error on tOpGetVariable");
        stack.push(env.variables[v.getString]);
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
        this.execute(cpyEnv.funcs[fname].func_body);
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
