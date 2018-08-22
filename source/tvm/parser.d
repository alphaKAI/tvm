module tvm.parser;
import pegged.grammar;
import std.format, std.string;
import std.conv;

mixin(grammar(`
PARSER:
  TopLevel < StatementList / Statement

  Declare < FunctionDeclare / VariableDeclare
  FunctionDeclare < "function" Symbol ParameterList Block
  Symbol < Identifier

  VariableDeclare < VariableDeclareWithAssign / VariableDeclareOnlySymbol
  VariableDeclareOnlySymbol < "var" LeftValue
  VariableDeclareWithAssign < "var" LeftValue "=" Value

  ParameterList < "()" / :"(" Parameter ("," Parameter)* :")"
  Parameter < Variable

  Value < LeftValue / RightValue
  LeftValue < Variable
  Variable < Identifier
  RightValue < Integer / StringLiteral / BooleanLiteral
  AssignExpression < LeftValue "=" Value
  ReturnExpression < "return" Expression
  MathExpression < AddExpression / SubExpression / MulExpression / DivExpression / ModExpression
  AddExpression < Value "+" Value
  SubExpression < Value "-" Value
  MulExpression < Value "*" Value
  DivExpression < Value "/" Value
  ModExpression < Value "%" Value
  CallExpression < Symbol ParameterList
  
  CompareExpression < EqualExpression / LtExpression / LteExpression / GtExpression / GteExpression
  EqualExpression < Value "==" Value
  LtExpression < Value "<" Value
  LteExpression < Value "<=" Value
  GtExpression < Value ">" Value
  GteExpression < Value ">=" Value

  LogicExpression < AndExpression / OrExpression / XorExpression
  AndExpression < Value "&&" Value
  OrExpression < Value "||" Value
  XorExpression < Value "^" Value

  Expression < CompareExpression / LogicExpression / AssignExpression / MathExpression / CallExpression / ReturnExpression / Value

  IFStatement < "if" :"(" Expression :")" Block ("else" Block)?

  Statement < FunctionDeclare / IFStatement / ((VariableDeclare / Expression) ";")
  StatementList < Statement+

  Block < "{" StatementList? "}"

  BooleanLiteral < "true" / "false"

  Integer <~ digit+
  Identifier <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
  Keyword <- "function" / "var" / "if" / "else" / "true" / "false"
  StringLiteral <~ doublequote (DQChar)* doublequote
  DQChar <- EscapeSequence / !doublequote .
  EscapeSequence <~ backslash ( quote
                              / doublequote
                              / backslash
                              / [abfnrtv]
                              )
`));

interface AST {
  string toString();
}

class Identifier : AST {
  string value;
  this(string value) {
    this.value = value;
  }

  override string toString() {
    return "Identifier <%s>".format(this.value);
  }
}

AST ident(string value) {
  return new Identifier(value);
}

class Symbol : AST {
  Identifier ident;
  this(Identifier ident) {
    this.ident = ident;
  }

  override string toString() {
    return "Symbol <%s>".format(ident.toString);
  }
}

AST symbol(Identifier ident) {
  return new Symbol(ident);
}

interface TopLevel : AST {
}

interface Statement : TopLevel {
}

interface Expression : Statement {
}

interface Value : AST, Expression {
}

interface LeftValue : Value {
}

class Variable : LeftValue {
  Identifier ident;
  this(Identifier ident) {
    this.ident = ident;
  }

  override string toString() {
    return "Variable <%s>".format(ident.toString);
  }
}

AST variable(Identifier ident) {
  return new Variable(ident);
}

interface RightValue : Value {
}

class StringLiteral : RightValue {
  string value;
  this(string value) {
    this.value = value;
  }

  override string toString() {
    return "StringLiteral <%s>".format(this.value);
  }
}

AST stringLit(string value) {
  return new StringLiteral(value);
}

class Integer : RightValue {
  long value;
  this(long value) {
    this.value = value;
  }

  override string toString() {
    return "Integer <%s>".format(this.value);
  }
}

AST integer(long value) {
  return new Integer(value);
}

class BooleanLiteral : RightValue {
  bool value;
  this(bool value) {
    this.value = value;
  }

  override string toString() {
    return "Bool <%s>".format(this.value);
  }
}

AST booleanLit(bool value) {
  return new BooleanLiteral(value);
}

class Parameter : AST {
  Variable var;
  this(Variable var) {
    this.var = var;
  }

  override string toString() {
    return "Parameter <%s>".format(var.toString);
  }
}

AST parameter(Variable var) {
  return new Parameter(var);
}

class ParameterList : AST {
  Parameter[] parameters;
  this(Parameter[] parameters) {
    this.parameters = parameters;
  }

  override string toString() {
    string[] s_parameters;
    foreach (parameter; parameters) {
      s_parameters ~= parameter.toString;
    }
    return "ParamterList <[%s]>".format(s_parameters.join(", "));
  }
}

AST parameterList(Parameter[] parameters) {
  return new ParameterList(parameters);
}

class StatementList : TopLevel {
  Statement[] statements;

  this(Statement[] statements) {
    this.statements = statements;
  }

  override string toString() {
    string[] s_statements;
    foreach (statement; statements) {
      s_statements ~= statement.toString;
    }
    return "StatementList <[%s]>".format(s_statements.join(", "));
  }
}

AST statementList(Statement[] statements) {
  return new StatementList(statements);
}

class Block : AST {
  StatementList statements;
  this() {
  }

  this(StatementList statements) {
    this.statements = statements;
  }

  override string toString() {
    if (statements !is null) {
      return "Block <%s>".format(statements.toString);
    } else {
      return "Block <Empty>";
    }
  }
}

class IFStatement : Statement {
  Expression cond;
  Block trueBlock;
  Block falseBlock;
  this(Expression cond, Block trueBlock) {
    this.cond = cond;
    this.trueBlock = trueBlock;
  }

  this(Expression cond, Block trueBlock, Block falseBlock) {
    this.cond = cond;
    this.trueBlock = trueBlock;
    this.falseBlock = falseBlock;
  }

  override string toString() {
    if (falseBlock is null) {
      return "IFStatement <cond: %s, trueBlock: %s>".format(cond.toString, trueBlock.toString);
    } else {
      return "IFStatement <cond: %s, trueBlock: %s, falseBlock: %s>".format(cond.toString,
          trueBlock.toString, falseBlock.toString);
    }
  }
}

AST ifStatement(Expression cond, Block trueBlock, Block falseBlock = null) {
  return new IFStatement(cond, trueBlock, falseBlock);
}

interface Declare : Statement {
}

class FunctionDeclare : Declare {
  Symbol symbol;
  ParameterList parameters;
  Block block;
  this(Symbol symbol, ParameterList parameters, Block block) {
    this.symbol = symbol;
    this.parameters = parameters;
    this.block = block;
  }

  override string toString() {
    return "FunctionDeclare <Name:%s, ParameterList: %s, Block: %s>".format(symbol.toString,
        parameters.toString, block.toString);
  }
}

interface VariableDeclare : Declare {
}

class VariableDeclareOnlySymbol : VariableDeclare {
  LeftValue lvalue;
  this(LeftValue lvalue) {
    this.lvalue = lvalue;
  }

  override string toString() {
    return "VariableDeclareOnlySymbol <%s>".format(lvalue.toString);
  }
}

class VariableDeclareWithAssign : VariableDeclare {
  LeftValue lvalue;
  Value value;
  this(LeftValue lvalue, Value value) {
    this.lvalue = lvalue;
    this.value = value;
  }

  override string toString() {
    return "VariableDeclareWithAssign <%s, %s>".format(lvalue.toString, value.toString);
  }
}

class AssignExpression : Expression {
  LeftValue lvalue;
  Value value;
  this(LeftValue lvalue, Value value) {
    this.lvalue = lvalue;
    this.value = value;
  }

  override string toString() {
    return "AssignExpression <%s, %s>".format(lvalue.toString, value.toString);
  }
}

AST assignExpression(LeftValue lvalue, Value value) {
  return new AssignExpression(lvalue, value);
}

interface MathExpression : Expression {
}

class AddExpression : MathExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "AddExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST addExpression(Value lvalue, Value rvalue) {
  return new AddExpression(lvalue, rvalue);
}

class SubExpression : MathExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "SubExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST subExpression(Value lvalue, Value rvalue) {
  return new SubExpression(lvalue, rvalue);
}

class MulExpression : MathExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "MulExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST mulExpression(Value lvalue, Value rvalue) {
  return new MulExpression(lvalue, rvalue);
}

class DivExpression : MathExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "DivExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST divExpression(Value lvalue, Value rvalue) {
  return new DivExpression(lvalue, rvalue);
}

class ModExpression : MathExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "ModExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST modExpression(Value lvalue, Value rvalue) {
  return new ModExpression(lvalue, rvalue);
}

class CallExpression : Expression {
  Symbol symbol;
  ParameterList parameters;
  this(Symbol symbol, ParameterList parameters) {
    this.symbol = symbol;
    this.parameters = parameters;
  }

  override string toString() {
    return "CallExpression <%s, ParameterList: %s>".format(symbol.toString, parameters.toString);
  }
}

AST callExpression(Symbol symbol, ParameterList parameters) {
  return new CallExpression(symbol, parameters);
}

class ReturnExpression : Expression {
  Expression expression;
  this(Expression expression) {
    this.expression = expression;
  }

  override string toString() {
    return "ReturnExpression <%s>".format(expression.toString);
  }
}

AST returnExpression(Expression expression) {
  return new ReturnExpression(expression);
}

interface CompareExpression : Expression {
}

class EqualExpression : CompareExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "CompareExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST equalExpression(Value lvalue, Value rvalue) {
  return new EqualExpression(lvalue, rvalue);
}

class LtExpression : CompareExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "LtExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST ltExpression(Value lvalue, Value rvalue) {
  return new LtExpression(lvalue, rvalue);
}

class LteExpression : CompareExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "LteExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST lteExpression(Value lvalue, Value rvalue) {
  return new LteExpression(lvalue, rvalue);
}

class GtExpression : CompareExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "GtExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST gtExpression(Value lvalue, Value rvalue) {
  return new GtExpression(lvalue, rvalue);
}

class GteExpression : CompareExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "GteExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST gteExpression(Value lvalue, Value rvalue) {
  return new GteExpression(lvalue, rvalue);
}

interface LogicExpression : Expression {
}

class AndExpression : LogicExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "AndExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST andExpression(Value lvalue, Value rvalue) {
  return new AndExpression(lvalue, rvalue);
}

class OrExpression : LogicExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "OrExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST orExpression(Value lvalue, Value rvalue) {
  return new OrExpression(lvalue, rvalue);
}

class XorExpression : LogicExpression {
  Value lvalue, rvalue;
  this(Value lvalue, Value rvalue) {
    this.lvalue = lvalue;
    this.rvalue = rvalue;
  }

  override string toString() {
    return "XorExpression <%s, %s>".format(lvalue.toString, rvalue.toString);
  }
}

AST xorExpression(Value lvalue, Value rvalue) {
  return new XorExpression(lvalue, rvalue);
}

AST buildAST(ParseTree p) {
  /*
  import std.stdio;

  writeln("p.name : ", p.name);
  */
  final switch (p.name) {
  case "PARSER":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.TopLevel":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.Declare":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.VariableDeclare":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.VariableDeclareOnlySymbol":
    auto e = p.children[0];
    LeftValue lvalue = cast(LeftValue)buildAST(e);
    assert(lvalue !is null, "Parse Error on VariableDeclareOnlySymbol");
    return new VariableDeclareOnlySymbol(lvalue);
  case "PARSER.VariableDeclareWithAssign":
    LeftValue e = cast(LeftValue)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on VariableDeclareWithAssign, <e>");
    Value e2 = cast(Value)buildAST(p.children[1]);
    assert(e2 !is null, "Parse Error on VariableDeclareWithAssign, <e2>");
    return new VariableDeclareWithAssign(e, e2);
  case "PARSER.FunctionDeclare":
    Symbol symbol = cast(Symbol)buildAST(p.children[0]);
    assert(symbol !is null, "Parse Error on FunctionDeclare, <symbol>");
    ParameterList params = cast(ParameterList)buildAST(p.children[1]);
    assert(params !is null, "Parse Error on FunctionDeclare, <params>");
    Block block = cast(Block)buildAST(p.children[2]);
    assert(block !is null, "Parse Error on FunctionDeclare, <block>");

    return new FunctionDeclare(symbol, params, block);
  case "PARSER.Symbol":
    Identifier e = cast(Identifier)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on Symbol");
    return symbol(e);
  case "PARSER.Value":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.LeftValue":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.Variable":
    Identifier e = cast(Identifier)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on Variable");
    return variable(e);
  case "PARSER.RightValue":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.Integer":
    auto e = p.matches[0];
    return integer(e.to!long);
  case "PARSER.StringLiteral":
    auto e = p.matches[0];
    return stringLit(e);
  case "PARSER.BooleanLiteral":
    auto e = p.matches[0];
    return booleanLit(e.to!bool);
  case "PARSER.ParameterList":
    auto params = p.children;
    Parameter[] s_params;
    foreach (param; params) {
      Parameter s_param = cast(Parameter)buildAST(param);
      assert(s_param !is null, "Parse Error on ParameterList");
      s_params ~= s_param;
    }

    return parameterList(s_params);
  case "PARSER.Parameter":
    Variable e = cast(Variable)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on Parameter");
    return parameter(e);
  case "PARSER.Statement":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.StatementList":
    auto statements = p.children;
    Statement[] s_statements;

    foreach (statement; statements) {
      Statement s_statement = cast(Statement)buildAST(statement);
      assert(s_statement !is null, "Parse Error on StatementList");
      s_statements ~= s_statement;
    }

    return statementList(s_statements);
  case "PARSER.Block":
    if (p.children.length) {
      StatementList e = cast(StatementList)buildAST(p.children[0]);
      assert(e !is null, "Parse Error on Block");
      return new Block(e);
    } else {
      return new Block();
    }
  case "PARSER.IFStatement":
    auto cond = cast(Expression)buildAST(p.children[0]);
    assert(cond !is null, "Parse Error on IFStatement <cond>");
    Block trueBlock = cast(Block)buildAST(p.children[1]);
    assert(trueBlock !is null, "Parse Error on IFStatement <trueBlock>");
    Block falseBlock;
    if (p.children.length == 3) {
      falseBlock = cast(Block)buildAST(p.children[2]);
      assert(falseBlock !is null, "Parse Error on IFStatement <falseBlock>");
    }
    return ifStatement(cond, trueBlock, falseBlock);
  case "PARSER.AssignExpression":
    LeftValue l = cast(LeftValue)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on AssignExpression<l>");
    Value v = cast(Value)buildAST(p.children[1]);
    assert(v !is null, "Parse Error on AssignExpression<v>");
    return assignExpression(l, v);
  case "PARSER.CompareExpression":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.EqualExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return equalExpression(l, r);
  case "PARSER.LtExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return ltExpression(l, r);
  case "PARSER.LteExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return lteExpression(l, r);
  case "PARSER.GtExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return gtExpression(l, r);
  case "PARSER.GteExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return gteExpression(l, r);
  case "PARSER.LogicExpression":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.AndExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return andExpression(l, r);
  case "PARSER.OrExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return orExpression(l, r);
  case "PARSER.XorExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return xorExpression(l, r);
  case "PARSER.MathExpression":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.AddExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return addExpression(l, r);
  case "PARSER.SubExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return subExpression(l, r);
  case "PARSER.MulExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return mulExpression(l, r);
  case "PARSER.DivExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return divExpression(l, r);
  case "PARSER.ModExpression":
    auto l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return modExpression(l, r);
  case "PARSER.CallExpression":
    Symbol funcSymbol = cast(Symbol)buildAST(p.children[0]);
    assert(funcSymbol !is null, "Parse Error on CallExpression<funcSymbol>");
    ParameterList params = cast(ParameterList)buildAST(p.children[1]);
    assert(params !is null, "Parse Error on CallExpression<params>");

    return callExpression(funcSymbol, params);
  case "PARSER.ReturnExpression":
    Expression e = cast(Expression)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on ReturnExpression");
    return returnExpression(e);
  case "PARSER.Expression":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.Identifier":
    auto e = p.matches[0];
    return ident(e);
  }
}
