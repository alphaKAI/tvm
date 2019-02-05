module tvm.deserializer;
import std.typecons, std.format, std.string, std.stdio, std.conv;
import std.exception;
import tvm.opcode, tvm.value;

bool table_initialized;
alias SerializedType = long;

OpcodeType[SerializedType] opnum_to_opcode;
ValueType[SerializedType] vnum_to_vtype;

void gen_tables() {
  if (!table_initialized) {
    enum code_for_opnum = {
      string[] code;
      static foreach (i, e; __traits(allMembers, OpcodeType)) {
        code ~= q{%d : OpcodeType.%s}.format(i, e);
      }
      return code;
    }();
    mixin("opnum_to_opcode = [%s];".format(code_for_opnum.join(", ")));

    enum code_for_vnum = {
      string[] code;
      static foreach (i, e; __traits(allMembers, ValueType)) {
        code ~= q{%d : ValueType.%s}.format(i, e);
      }
      return code;
    }();

    mixin("vnum_to_vtype = [%s];".format(code_for_vnum.join(", ")));

    table_initialized = true;
  }
}

static this() {
  gen_tables();
}

Tuple!(IValue[], size_t) deserialize(SerializedType[] serialized, size_t elem_size = size_t.max) {
  IValue[] deserialized;
  size_t idx;

  for (; idx < serialized.length && idx < elem_size; idx++) {
    // Emit type of elem
    ValueType vtype = vnum_to_vtype[serialized[idx++]];
    final switch (vtype) with (ValueType) {
    case Long:
      deserialized ~= new IValue(serialized[idx++]);
      break;
    case String:
      size_t len = cast(size_t)serialized[idx++];
      string str;
      foreach (_; 0 .. len) {
        str ~= serialized[idx++].to!char;
      }
      deserialized ~= new IValue(str);
      break;
    case Bool:
      deserialized ~= new IValue(serialized[idx++].to!bool);
      break;
    case Array:
      size_t len = cast(size_t)serialized[idx++];
      auto ret = deserialize(serialized[idx .. $], len);
      IValue[] arr = ret[0];
      idx += ret[1];
      deserialized ~= new IValue(arr);
      break;
    case Function:
      throw new Error("Unsupported");
    case Null:
      deserialized ~= new IValue();
      break;
    }
  }
  return tuple(deserialized, idx);
}

Opcode[] deserialize(SerializedType[] serialized) {
  Opcode[] code;

  void procWith1Arg(Opcode type, ref size_t idx) {
    code ~= type;
    auto ret = deserialize(serialized[idx .. $], 1);
    Opcode[] operands;
    foreach (operand; ret[0]) {
      operands ~= cast(Opcode)operand;
    }
    code ~= operands;
    idx += ret[1] - 1;
  }

  void procWith2Arg(Opcode type, ref size_t idx) {
    code ~= type;

    auto ret = deserialize(serialized[idx .. $], 1);
    Opcode[] operands;
    foreach (operand; ret[0]) {
      operands ~= cast(Opcode)operand;
    }
    code ~= operands;
    idx += ret[1] - 1;

    ret = deserialize(serialized[idx .. $], 1);
    operands = [];
    foreach (operand; ret[0]) {
      operands ~= cast(Opcode)operand;
    }
    code ~= operands;
    idx += ret[1] - 1;
  }

  for (size_t idx; idx < serialized.length;) {
    OpcodeType type = opnum_to_opcode[serialized[idx++]];
    final switch (type) with (OpcodeType) {
    case tOpVariableDeclareOnlySymbol:
      procWith1Arg(opVariableDeclareOnlySymbol, idx);
      break;
    case tOpVariableDeclareWithAssign:
      procWith1Arg(opVariableDeclareWithAssign, idx);
      break;
    case tOpPop:
      throw new Error("<Deserialize> Not supported %s".format(type));
    case tOpPush:
      procWith1Arg(opPush, idx);
      break;
    case tOpAdd:
      code ~= opAdd;
      break;
    case tOpSub:
      code ~= opSub;
      break;
    case tOpMul:
      code ~= opMul;
      break;
    case tOpDiv:
      code ~= opMul;
      break;
    case tOpMod:
      code ~= opMul;
      break;
    case tOpReturn:
      code ~= opReturn;
      break;
    case tOpGetVariable:
      procWith1Arg(opGetVariable, idx);
      break;
    case tOpSetVariablePop:
      procWith1Arg(opSetVariablePop, idx);
      break;
    case tOpSetArrayElement:
      procWith1Arg(opSetArrayElement, idx);
      break;
    case tOpGetArrayElement:
      procWith1Arg(opGetArrayElement, idx);
      break;
    case tOpMakeArray:
      procWith1Arg(opMakeArray, idx);
      break;
    case tOpCall:
      procWith1Arg(opCall, idx);
      break;
    case tOpNop:
      code ~= opNop;
      idx++;
      break;
    case tOpFunctionDeclare:
      procWith2Arg(opFunctionDeclare, idx);
      break;
    case tOpEqualExpression:
      code ~= opEqualExpression;
      break;
    case tOpNotEqualExpression:
      code ~= opNotEqualExpression;
      break;
    case tOpLtExpression:
      code ~= opLtExpression;
      break;
    case tOpLteExpression:
      code ~= opLteExpression;
      break;
    case tOpGtExpression:
      code ~= opGtExpression;
      break;
    case tOpGteExpression:
      code ~= opGteExpression;
      break;
    case tOpAndExpression:
      code ~= opAndExpression;
      break;
    case tOpOrExpression:
      code ~= opOrExpression;
      break;
    case tOpXorExpression:
      code ~= opXorExpression;
      break;
    case tOpJumpRel:
      procWith1Arg(opJumpRel, idx);
      break;
    case tOpJumpAbs:
      procWith1Arg(opJumpAbs, idx);
      break;
    case tOpPrint:
      code ~= opPrint;
      break;
    case tOpPrintln:
      code ~= opPrintln;
      break;
    case tOpIFStatement:
      procWith1Arg(opIFStatement, idx);
      break;
    case tOpAssignExpression:
      procWith1Arg(opAssignExpression, idx);
      break;
    case tOpAssert:
      code ~= opAssert;
      break;
    case tIValue:
      throw new Error("<Deserialize> Not supported %s".format(type));
    }
  }

  return code;
}

Opcode[] readFromFile(string filename) {
  auto f = File(filename, "rb");
  SerializedType[] buf;
  buf.length = f.size / SerializedType.sizeof;
  f.rawRead(buf);
  return deserialize(buf);
}
