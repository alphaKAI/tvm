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

class Parens : Expression {
  Expression expression;

  this(Expression expression) {
    this.expression = expression;
  }

  override string toString() {
    return "Parens <%s>".format(this.expression.toString);
  }
}

AST parens(Expression expression) {
  return new Parens(expression);
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
  Expression expr;
  this(Expression expr) {
    this.expr = expr;
  }

  override string toString() {
    return "Parameter <%s>".format(expr.toString);
  }
}

AST parameter(Expression expr) {
  return new Parameter(expr);
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
  Expression expr;
  this(LeftValue lvalue, Expression expr) {
    this.lvalue = lvalue;
    this.expr = expr;
  }

  override string toString() {
    return "VariableDeclareWithAssign <%s, %s>".format(lvalue.toString, expr.toString);
  }
}

class AssignExpression : Expression {
  LeftValue lvalue;
  Expression expr;
  this(LeftValue lvalue, Expression expr) {
    this.lvalue = lvalue;
    this.expr = expr;
  }

  override string toString() {
    return "AssignExpression <%s, %s>".format(lvalue.toString, expr.toString);
  }
}

AST assignExpression(LeftValue lvalue, Expression expr) {
  return new AssignExpression(lvalue, expr);
}

interface MathExpression : Expression {
}

class AddExpression : MathExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "AddExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST addExpression(Expression lexpr, Expression rexpr) {
  return new AddExpression(lexpr, rexpr);
}

class SubExpression : MathExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "SubExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST subExpression(Expression lexpr, Expression rexpr) {
  return new SubExpression(lexpr, rexpr);
}

class MulExpression : MathExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "MulExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST mulExpression(Expression lexpr, Expression rexpr) {
  return new MulExpression(lexpr, rexpr);
}

class DivExpression : MathExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "DivExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST divExpression(Expression lexpr, Expression rexpr) {
  return new DivExpression(lexpr, rexpr);
}

class ModExpression : MathExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "ModExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST modExpression(Expression lexpr, Expression rexpr) {
  return new ModExpression(lexpr, rexpr);
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
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "CompareExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST equalExpression(Expression lexpr, Expression rexpr) {
  return new EqualExpression(lexpr, rexpr);
}

class LtExpression : CompareExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "LtExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST ltExpression(Expression lexpr, Expression rexpr) {
  return new LtExpression(lexpr, rexpr);
}

class LteExpression : CompareExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "LteExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST lteExpression(Expression lexpr, Expression rexpr) {
  return new LteExpression(lexpr, rexpr);
}

class GtExpression : CompareExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "GtExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST gtExpression(Expression lexpr, Expression rexpr) {
  return new GtExpression(lexpr, rexpr);
}

class GteExpression : CompareExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "GteExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST gteExpression(Expression lexpr, Expression rexpr) {
  return new GteExpression(lexpr, rexpr);
}

interface LogicExpression : Expression {
}

class AndExpression : LogicExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "AndExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST andExpression(Expression lexpr, Expression rexpr) {
  return new AndExpression(lexpr, rexpr);
}

class OrExpression : LogicExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "OrExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST orExpression(Expression lexpr, Expression rexpr) {
  return new OrExpression(lexpr, rexpr);
}

class XorExpression : LogicExpression {
  Expression lexpr, rexpr;
  this(Expression lexpr, Expression rexpr) {
    this.lexpr = lexpr;
    this.rexpr = rexpr;
  }

  override string toString() {
    return "XorExpression <%s, %s>".format(lexpr.toString, rexpr.toString);
  }
}

AST xorExpression(Expression lexpr, Expression rexpr) {
  return new XorExpression(lexpr, rexpr);
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
    Expression e2 = cast(Expression)buildAST(p.children[1]);
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
    Expression e = cast(Expression)buildAST(p.children[0]);
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
    Expression v = cast(Expression)buildAST(p.children[1]);
    assert(v !is null, "Parse Error on AssignExpression<v>");
    return assignExpression(l, v);
  case "PARSER.CompareExpression":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.EqualExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return equalExpression(l, r);
  case "PARSER.LtExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return ltExpression(l, r);
  case "PARSER.LteExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return lteExpression(l, r);
  case "PARSER.GtExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return gtExpression(l, r);
  case "PARSER.GteExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return gteExpression(l, r);
  case "PARSER.LogicExpression":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.AndExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return andExpression(l, r);
  case "PARSER.OrExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return orExpression(l, r);
  case "PARSER.XorExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return xorExpression(l, r);
  case "PARSER.MathExpression":
    auto e = p.children[0];
    return buildAST(e);
  case "PARSER.AddExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return addExpression(l, r);
  case "PARSER.SubExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return subExpression(l, r);
  case "PARSER.MulExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return mulExpression(l, r);
  case "PARSER.DivExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on %s<r>".format(p.name));
    return divExpression(l, r);
  case "PARSER.ModExpression":
    auto l = cast(Expression)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on %s<l>".format(p.name));
    auto r = cast(Expression)buildAST(p.children[1]);
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
  case "PARSER.Parens":
    auto e = cast(Expression)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on Parens");
    return parens(e);
  case "PARSER.Identifier":
    auto e = p.matches[0];
    return ident(e);
  }
}
