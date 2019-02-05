module tvm.compiler;
import tvm.parser, tvm.opcode, tvm.value;
import std.format, std.conv;

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
    return opGetVariable ~ compileASTtoOpcode(var.ident);
  case tStringLiteral:
    auto value = cast(StringLiteral)ast;
    return [opPush, new IValue(value.value)];
  case tInteger:
    auto value = cast(Integer)ast;
    return [opPush, new IValue(value.value)];
  case tBooleanLiteral:
    auto value = cast(BooleanLiteral)ast;
    return [opPush, new IValue(value.value)];
  case tArrayLiteral:
    auto value = cast(ArrayLiteral)ast;
    Opcode[] elements;
    size_t array_size;
    foreach (elem; value.value) {
      elements ~= compileASTtoOpcode(elem);
      array_size++;
    }
    return elements ~ opMakeArray ~ new IValue(array_size);
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

    return compileASTtoOpcode(cond) ~ opIFStatement ~ (
        new IValue(trueBlockLength)) ~ op_trueBlock ~ op_falseBlock;
  case tForStatement:
    auto forStmt = cast(ForStatement)ast;
    assert(forStmt !is null, "Compile Error on <%s>".format(ast.type));
    VariableDeclare vassign = forStmt.vassign;
    Expression cond = forStmt.cond, update = forStmt.update;
    Block block = forStmt.block;

    Opcode[] op_vassign = compileASTtoOpcode(vassign);
    Opcode[] loop_body;
    Opcode[] op_cond = compileASTtoOpcode(cond);
    Opcode[] op_update = compileASTtoOpcode(update);
    Opcode[] op_block = compileASTtoOpcode(block);

    loop_body = op_block ~ op_update;
    loop_body ~= [opJumpRel, new IValue(-(loop_body.length.to!long + op_cond.length.to!long + 4))];
    return op_vassign ~ op_cond ~ opIFStatement ~ (new IValue(loop_body.length.to!long)) ~ loop_body;
  case tWhileStatement:
    auto whileStmt = cast(WhileStatement)ast;
    assert(whileStmt !is null, "Compile Error on <%s>".format(ast.type));
    Expression cond = whileStmt.cond;
    Block block = whileStmt.block;

    Opcode[] loop_body;
    Opcode[] op_cond = compileASTtoOpcode(cond);
    Opcode[] op_block = compileASTtoOpcode(block);

    loop_body ~= op_block;
    loop_body ~= [opJumpRel, new IValue(-(op_block.length.to!long + op_cond.length.to!long + 4))];
    return op_cond ~ opIFStatement ~ (new IValue(loop_body.length.to!long)) ~ loop_body;
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

    long op_blocks_length = op_blocks.length.to!long;

    auto ret = opFunctionDeclare ~ compileASTtoOpcode(symbol) ~ (
        new IValue(op_blocks_length)) ~ op_blocks;
    return ret;
  case tVariableDeclareOnlySymbol:
    auto var = cast(VariableDeclareOnlySymbol)ast;
    assert(var !is null, "Compile Error on <%s>".format(ast.type));
    auto l = compileASTtoOpcode(var.lvalue);
    if (l.length == 2 && l[0].type == OpcodeType.tOpGetVariable) {
      l = l[1 .. $];
    }
    return opVariableDeclareOnlySymbol ~ l;
  case tVariableDeclareWithAssign:
    auto var = cast(VariableDeclareWithAssign)ast;
    assert(var !is null, "Compile Error on <%s>".format(ast.type));
    auto l = compileASTtoOpcode(var.lvalue);
    if (l.length == 2 && l[0].type == OpcodeType.tOpGetVariable) {
      l = l[1 .. $];
    }
    auto e = compileASTtoOpcode(var.expr);

    return e ~ opVariableDeclareWithAssign ~ l;
  case tAssignExpression:
    auto assign = cast(AssignExpression)ast;
    auto l = compileASTtoOpcode(assign.lvalue);
    if (l.length == 2 && l[0].type == OpcodeType.tOpGetVariable) {
      l = l[1 .. $];
    } else {
      throw new Error("Compile Error on <%s>".format(ast.type));
    }
    auto e = compileASTtoOpcode(assign.expr);

    return e ~ opAssignExpression ~ l;
  case tAddExpression:
    auto expr = cast(AddExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opAdd;
  case tSubExpression:
    auto expr = cast(SubExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opSub;
  case tMulExpression:
    auto expr = cast(MulExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opMul;
  case tDivExpression:
    auto expr = cast(DivExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opDiv;
  case tModExpression:
    auto expr = cast(ModExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opMod;
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
    return r ~ l ~ opEqualExpression;
  case tNotEqualExpression:
    auto expr = cast(NotEqualExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opNotEqualExpression;
  case tLtExpression:
    auto expr = cast(LtExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opLtExpression;
  case tLteExpression:
    auto expr = cast(LteExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opLteExpression;
  case tGtExpression:
    auto expr = cast(GtExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opGtExpression;
  case tGteExpression:
    auto expr = cast(GteExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opGteExpression;
  case tAndExpression:
    auto expr = cast(AndExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opAndExpression;
  case tOrExpression:
    auto expr = cast(OrExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opOrExpression;
  case tXorExpression:
    auto expr = cast(XorExpression)ast;
    assert(expr !is null, "Compile Error on <%s>".format(ast.type));
    Opcode[] r = compileASTtoOpcode(expr.rexpr), l = compileASTtoOpcode(expr.lexpr);
    return r ~ l ~ opXorExpression;
  case tArrayElementSetExpression:
    auto arrayElemSetExpr = cast(ArrayElementSetExpression)ast;
    Opcode[] variable_op = compileASTtoOpcode(arrayElemSetExpr.variable);
    if (variable_op.length == 2 && variable_op[0].type == OpcodeType.tOpGetVariable) {
      variable_op = variable_op[1 .. $];
    } else {
      throw new Error("Compile Error on <%s>".format(ast.type));
    }
    Opcode[] idx_op = compileASTtoOpcode(arrayElemSetExpr.idx);
    Opcode[] rexpr_op = compileASTtoOpcode(arrayElemSetExpr.rexpr);

    return rexpr_op ~ idx_op ~ opSetArrayElement ~ variable_op;
  case tArrayElementGetExpression:
    auto arrayElemGetExpr = cast(ArrayElementGetExpression)ast;
    Opcode[] variable_op = compileASTtoOpcode(arrayElemGetExpr.variable);
    if (variable_op.length == 2 && variable_op[0].type == OpcodeType.tOpGetVariable) {
      variable_op = variable_op[1 .. $];
    } else {
      throw new Error("Compile Error on <%s>".format(ast.type));
    }
    Opcode[] idx_op = compileASTtoOpcode(arrayElemGetExpr.idx);

    return idx_op ~ opGetArrayElement ~ variable_op;
  case tAssertExpression:
    auto assertExpression = cast(AssertExpression)ast;
    Opcode[] cond_ops = compileASTtoOpcode(assertExpression.cond);
    Opcode[] msg_ops = compileASTtoOpcode(assertExpression.msg);

    return cond_ops ~ msg_ops ~ opAssert;
  }
}
