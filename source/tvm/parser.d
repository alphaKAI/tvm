module tvm.parser;
import pegged.grammar;
import std.format, std.string;
import std.conv;

mixin(grammar(`
JS:
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
  RightValue < Integer / StringLiteral
  AssignExpression < LeftValue "=" Value
  ReturnExpression < "return" Expression
  MathExpression < AddExpression / SubExpression / MulExpression / DivExpression / ModExpression
  AddExpression < Value "+" Value
  SubExpression < Value "-" Value
  MulExpression < Value "*" Value
  DivExpression < Value "/" Value
  ModExpression < Value "%" Value
  CallExpression < Symbol ParameterList
  Expression < AssignExpression / MathExpression / CallExpression / ReturnExpression / Value

  Statement < FunctionDeclare / ((VariableDeclare / Expression) ";")
  StatementList < Statement+

  Block < "{" StatementList? "}"

  Integer <~ digit+
  Identifier <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
  Keyword <- "function" "var"
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

AST buildAST(ParseTree p) {
  //writeln("p.name : ", p.name);
  final switch (p.name) {
  case "JS":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.TopLevel":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.Declare":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.VariableDeclare":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.VariableDeclareOnlySymbol":
    auto e = p.children[0];
    LeftValue lvalue = cast(LeftValue)buildAST(e);
    assert(lvalue !is null, "Parse Error on VariableDeclareOnlySymbol");
    return new VariableDeclareOnlySymbol(lvalue);
  case "JS.VariableDeclareWithAssign":
    LeftValue e = cast(LeftValue)buildAST(p.children[0]);
    Value e2 = cast(Value)buildAST(p.children[1]);
    assert(e !is null, "Parse Error on VariableDeclareWithAssign, <e>");
    assert(e2 !is null, "Parse Error on VariableDeclareWithAssign, <e2>");
    return new VariableDeclareWithAssign(e, e2);
  case "JS.FunctionDeclare":
    Symbol symbol = cast(Symbol)buildAST(p.children[0]);
    ParameterList params = cast(ParameterList)buildAST(p.children[1]);
    Block block = cast(Block)buildAST(p.children[2]);

    assert(symbol !is null, "Parse Error on FunctionDeclare, <symbol>");
    assert(params !is null, "Parse Error on FunctionDeclare, <params>");
    assert(block !is null, "Parse Error on FunctionDeclare, <block>");

    return new FunctionDeclare(symbol, params, block);
  case "JS.Symbol":
    Identifier e = cast(Identifier)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on Symbol");
    return symbol(e);
  case "JS.Value":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.LeftValue":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.Variable":
    Identifier e = cast(Identifier)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on Variable");
    return variable(e);
  case "JS.RightValue":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.Integer":
    auto e = p.matches[0];
    return integer(e.to!long);
  case "JS.StringLiteral":
    auto e = p.matches[0];
    return stringLit(e);
  case "JS.ParameterList":
    auto params = p.children;
    Parameter[] s_params;
    foreach (param; params) {
      Parameter s_param = cast(Parameter)buildAST(param);
      assert(s_param !is null, "Parse Error on ParameterList");
      s_params ~= s_param;
    }

    return parameterList(s_params);
  case "JS.Parameter":
    Variable e = cast(Variable)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on Parameter");
    return parameter(e);
  case "JS.Statement":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.StatementList":
    auto statements = p.children;
    Statement[] s_statements;

    foreach (statement; statements) {
      Statement s_statement = cast(Statement)buildAST(statement);
      assert(s_statement !is null, "Parse Error on StatementList");
      s_statements ~= s_statement;
    }

    return statementList(s_statements);
  case "JS.Block":
    if (p.children.length) {
      StatementList e = cast(StatementList)buildAST(p.children[0]);
      assert(e !is null, "Parse Error on Block");
      return new Block(e);
    } else {
      return new Block();
    }
  case "JS.AssignExpression":
    LeftValue l = cast(LeftValue)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on AssignExpression<l>");
    Value v = cast(Value)buildAST(p.children[1]);
    assert(v !is null, "Parse Error on AssignExpression<v>");
    return assignExpression(l, v);
  case "JS.MathExpression":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.AddExpression":
    Value l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on AddExpression<l>");
    Value r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on AddExpression<r>");
    return addExpression(l, r);
  case "JS.SubExpression":
    Value l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on SubExpression<l>");
    Value r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on SubExpression<r>");
    return subExpression(l, r);
  case "JS.MulExpression":
    Value l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on MulExpression<l>");
    Value r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on MulExpression<r>");
    return mulExpression(l, r);
  case "JS.DivExpression":
    Value l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on DivExpression<l>");
    Value r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on DivExpression<r>");
    return divExpression(l, r);
  case "JS.ModExpression":
    Value l = cast(Value)buildAST(p.children[0]);
    assert(l !is null, "Parse Error on ModExpression<l>");
    Value r = cast(Value)buildAST(p.children[1]);
    assert(r !is null, "Parse Error on ModExpression<r>");
    return modExpression(l, r);
  case "JS.CallExpression":
    Symbol funcSymbol = cast(Symbol)buildAST(p.children[0]);
    assert(funcSymbol !is null, "Parse Error on CallExpression<funcSymbol>");
    ParameterList params = cast(ParameterList)buildAST(p.children[1]);
    assert(params !is null, "Parse Error on CallExpression<params>");

    return callExpression(funcSymbol, params);
  case "JS.ReturnExpression":
    Expression e = cast(Expression)buildAST(p.children[0]);
    assert(e !is null, "Parse Error on ReturnExpression");
    return returnExpression(e);
  case "JS.Expression":
    auto e = p.children[0];
    return buildAST(e);
  case "JS.Identifier":
    auto e = p.matches[0];
    return ident(e);
  }
}
