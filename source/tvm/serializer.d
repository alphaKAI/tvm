module tvm.serializer;
import std.format, std.string, std.stdio, std.conv;
import tvm.opcode, tvm.value;

bool table_initialized;
alias SerializedType = long;

SerializedType[OpcodeType] opcode_to_opnum;
SerializedType[ValueType] vtype_to_vnum;

void gen_tables() {
  if (!table_initialized) {
    enum code_for_opnum = {
      string[] code;
      static foreach (i, e; __traits(allMembers, OpcodeType)) {
        code ~= q{OpcodeType.%s : %d}.format(e, i);
      }
      return code;
    }();
    mixin("opcode_to_opnum = [%s];".format(code_for_opnum.join(", ")));

    enum code_for_vnum = {
      string[] code;
      static foreach (i, e; __traits(allMembers, ValueType)) {
        code ~= q{ValueType.%s : %d}.format(e, i);
      }
      return code;
    }();

    mixin("vtype_to_vnum = [%s];".format(code_for_vnum.join(", ")));

    table_initialized = true;
  }
}

static this() {
  gen_tables();
}

SerializedType[] serialize(IValue[] ivalues) {
  SerializedType[] serialized;
  foreach (e; ivalues) {
    // Emit type of elem
    serialized ~= vtype_to_vnum[e.vtype];
    final switch (e.vtype) with (ValueType) {
    case Long:
      serialized ~= e.getLong;
      break;
    case String:
      auto s = e.getString;
      SerializedType[] sls;
      foreach (ch; s) {
        sls ~= ch.to!SerializedType;
      }
      // Emit size of str
      serialized ~= cast(SerializedType)s.length;
      serialized ~= sls;
      break;
    case Bool:
      serialized ~= e.getBool.to!long;
      break;
    case Array:
      auto arr = e.getArray;
      auto arr_serialized = serialize(arr);

      serialized ~= cast(SerializedType)arr.length;
      serialized ~= arr_serialized;
      break;
    case Function:
      throw new Error("Unsupported");
    case Null:
      break;
    }
  }
  return serialized;
}

SerializedType[] serialize(Opcode[] code) {
  SerializedType[] serialized;

  for (size_t idx; idx < code.length; idx++) {
    auto ins = code[idx];
    auto type = ins.type;
    final switch (type) with (OpcodeType) {
    case tOpVariableDeclareOnlySymbol:
    case tOpVariableDeclareWithAssign:
      serialized ~= opcode_to_opnum[type];
      IValue var_name = cast(IValue)code[++idx];
      serialized ~= serialize([var_name]);
      break;
    case tOpPop:
      serialized ~= opcode_to_opnum[type];
      break;
    case tOpPush:
      // Emit type of ins
      serialized ~= opcode_to_opnum[type];
      IValue e = cast(IValue)code[++idx];
      serialized ~= serialize([e]);
      break;
    case tOpAdd:
    case tOpSub:
    case tOpMul:
    case tOpDiv:
    case tOpMod:
    case tOpReturn:
      serialized ~= opcode_to_opnum[type];
      break;
    case tOpGetVariable:
    case tOpSetVariablePop:
    case tOpSetArrayElement:
    case tOpGetArrayElement:
      serialized ~= opcode_to_opnum[type];
      IValue var_name = cast(IValue)code[++idx];
      serialized ~= serialize([var_name]);
      break;
    case tOpMakeArray:
    case tOpCall:
      serialized ~= opcode_to_opnum[type];
      IValue e = cast(IValue)code[++idx];
      serialized ~= serialize([e]);
      break;
    case tOpNop:
      serialized ~= opcode_to_opnum[type];
      break;
    case tOpFunctionDeclare:
      serialized ~= opcode_to_opnum[type];
      IValue func_name = cast(IValue)code[++idx];
      serialized ~= serialize([func_name]);
      IValue block_len = cast(IValue)code[++idx];
      serialized ~= serialize([block_len]);
      break;
    case tOpEqualExpression:
    case tOpNotEqualExpression:
    case tOpLtExpression:
    case tOpLteExpression:
    case tOpGtExpression:
    case tOpGteExpression:
    case tOpAndExpression:
    case tOpOrExpression:
    case tOpXorExpression:
      serialized ~= opcode_to_opnum[type];
      break;
    case tOpJumpRel:
    case tOpJumpAbs:
      serialized ~= opcode_to_opnum[type];
      IValue e = cast(IValue)code[++idx];
      serialized ~= serialize([e]);
      break;
    case tOpPrint:
    case tOpPrintln:
      serialized ~= opcode_to_opnum[type];
      break;
    case tOpIFStatement:
      serialized ~= opcode_to_opnum[type];
      IValue true_block_len = cast(IValue)code[++idx];
      serialized ~= serialize([true_block_len]);
      break;
    case tOpAssignExpression:
      serialized ~= opcode_to_opnum[type];
      IValue var_name = cast(IValue)code[++idx];
      serialized ~= serialize([var_name]);
      break;
    case tOpAssert:
      serialized ~= opcode_to_opnum[type];
      break;
    case tIValue:
      throw new Error("<Serialize> Not supported %s".format(type));
    }
  }

  return serialized;
}

void saveToFile(Opcode[] code, string filename) {
  auto serialized = serialize(code);
  auto f = File(filename, "wb");
  f.rawWrite(serialized);
}
