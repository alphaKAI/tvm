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

class OpVariableDeclareWithAssign : Opcode {
  mixin(genTypeMethod!(typeof(this)));
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

Opcode[] compileASTtoOpcode(AST ast) {
  final switch (ast.type) with (ASTType) {
  case tIdentifier:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tSymbol:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tParens:
    auto parens = cast(Parens)ast;
    assert(parens !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(parens.expression);
  case tVariable:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tStringLiteral:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tInteger:
    auto value = cast(Integer)ast;
    return [opPush, new IValue(value.value)];
  case tBooleanLiteral:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tParameter:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tParameterList:
    throw new Error("Not Implemented <%s>".format(ast.type));
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
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tVariableDeclareWithAssign:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tAssignExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tAddExpression:
    auto expr = cast(AddExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opAdd];
  case tSubExpression:
    auto expr = cast(SubExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opSub];
  case tMulExpression:
    auto expr = cast(MulExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opMul];
  case tDivExpression:
    auto expr = cast(DivExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opDiv];
  case tModExpression:
    auto expr = cast(ModExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    return compileASTtoOpcode(expr.rexpr) ~ compileASTtoOpcode(expr.lexpr) ~ [opMod];
  case tCallExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
  case tReturnExpression:
    throw new Error("Not Implemented <%s>".format(ast.type));
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

  this() {
    this.env = new Env;
    this.registers = new Registers;
  }

  IValue execute(Opcode[] code) {
    Stack!IValue stack;

    for (size_t pc; pc < code.length; pc++) {
      Opcode op = code[pc];
      final switch (op.type) with (OpcodeType) {
      case tOpVariableDeclareOnlySymbol:
        throw new Error("Not Implemented <%s>".format(op.type));
      case tOpVariableDeclareWithAssign:
        throw new Error("Not Implemented <%s>".format(op.type));
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
      case tIValue:
        throw new Error("Not Implemented <%s>".format(op.type));
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
  MathExpression < AddExpression / SubExpression / MulExpression / DivExpression / ModExpression
  AddExpression < Expression "+" Expression
  SubExpression < Expression "-" Expression
  MulExpression < Expression "*" Expression
  DivExpression < Expression "/" Expression
  ModExpression < Expression "%" Expression
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
