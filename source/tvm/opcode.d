module tvm.opcode;
import std.format;

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
  tOpSetVariableI,
  tOpSetVariablePop,
  tOpCall,
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
  tOpAssignExpression,
  tIValue
}

interface Opcode {
  OpcodeType type();
}

string genTypeMethod(T)() {
  return q{
    OpcodeType type() {
      return OpcodeType.t%s;
    }
  }.format(T.stringof);
}

/*
  genOpCodeClassAndHelper willgenerate...
  genOpCodeClassAndHelper!"tOpPop"
  ↓
  class OpPop : Opcode {
    mixin(genTypeMethod!(typeof(this)));
  }
  Opcode opPop() {
    static Opcode ret;
    if (ret is null) {
      ret = new OpPop;
    }
    return ret;
  }
 */
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
  static Opcode ret;
  if (ret is null) {
    ret = new %s;
  }
  return ret;
}
}.format(base, helper, base);
}

/*
  Generate all Instructions classes and helper function of them.
 */
static foreach (elem; __traits(allMembers, OpcodeType)) {
  mixin(genOpCodeClassAndHelper!(elem));
}
