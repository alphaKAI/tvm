module tvm.value;
import std.exception;
import std.traits;
import std.conv;
import std.algorithm, std.array, std.format;
import tvm.opcode;

enum ValueType {
  Long,
  String,
  Bool,
  Array,
  Null
}

class IValue : Opcode {
  ValueType vtype;

  private {
    long long_value;
    string string_value;
    bool bool_value;
    IValue[] array_value;
  }

  this() {
    this.vtype = ValueType.Null;
  }

  this(ValueType vtype) {
    this.vtype = vtype;
  }

  this(long value) {
    this.opAssign(value);
  }

  this(string value) {
    this.opAssign(value);
  }

  this(bool value) {
    this.opAssign(value);
  }

  this(IValue[] value) {
    this.opAssign(value);
  }

  long getLong() {
    enforce(this.vtype == ValueType.Long);
    return this.long_value;
  }

  string getString() {
    enforce(this.vtype == ValueType.String);
    return this.string_value;
  }

  bool getBool() {
    enforce(this.vtype == ValueType.Bool);
    return this.bool_value;
  }

  IValue[] getArray() {
    enforce(this.vtype == ValueType.Array);
    return this.array_value;
  }

  auto getNull() {
    throw new Exception("Can't get from NULL value");
  }

  void opAssign(long value) {
    this.init;
    this.long_value = value;
    this.vtype = ValueType.Long;
  }

  void opAssign(string value) {
    this.init;
    this.string_value = value;
    this.vtype = ValueType.String;
  }

  void opAssign(bool value) {
    this.init;
    this.bool_value = value;
    this.vtype = ValueType.Bool;
  }

  void opAssign(IValue[] value) {
    this.init;
    this.array_value = value;
    this.vtype = ValueType.Array;
  }

  override string toString() {
    final switch (this.vtype) with (ValueType) {
    case Long:
      return this.long_value.to!string;
    case String:
      return "\"%s\"".format(this.string_value);
    case Bool:
      return this.bool_value.to!string;
    case Array:
      string[] array_str;
      foreach (elem; this.array_value) {
        array_str ~= elem.toString;
      }
      return "[" ~ array_str.join(", ") ~ "]";
    case Null:
      return "null";
    }
  }

  void addTo(IValue value) {
    enforce(this.vtype == value.vtype && value.vtype == ValueType.Long);
    this.long_value += value.getLong;
  }

  void subTo(IValue value) {
    enforce(this.vtype == value.vtype && value.vtype == ValueType.Long);
    this.long_value -= value.getLong;
  }

  void mulTo(IValue value) {
    enforce(this.vtype == value.vtype && value.vtype == ValueType.Long);
    this.long_value *= value.getLong;
  }

  void divTo(IValue value) {
    enforce(this.vtype == value.vtype && value.vtype == ValueType.Long);
    this.long_value /= value.getLong;
  }

  void modTo(IValue value) {
    enforce(this.vtype == value.vtype && value.vtype == ValueType.Long);
    this.long_value %= value.getLong;
  }

  IValue opBinary(string op)(IValue value) if (op == "+") {
    enforce(value.vtype == ValueType.Long);
    return new IValue(this.long_value + value.getLong);
  }

  IValue opBinary(string op)(IValue value) if (op == "-") {
    enforce(value.vtype == ValueType.Long);

    return new IValue(this.long_value - value.getLong);
  }

  IValue opBinary(string op)(IValue value) if (op == "*") {
    enforce(value.vtype == ValueType.Long);
    return new IValue(this.long_value * value.getLong);
  }

  IValue opBinary(string op)(IValue value) if (op == "/") {
    enforce(value.vtype == ValueType.Long);
    return new IValue(this.long_value / value.getLong);
  }

  IValue opBinary(string op)(IValue value) if (op == "%") {
    enforce(value.vtype == ValueType.Long);
    return new IValue(this.long_value % value.getLong);
  }

  void init() {
    if (this.vtype != ValueType.Null) {
      if (this.vtype == ValueType.Long) {
        this.long_value = 0;
      }
      if (this.vtype == ValueType.String) {
        this.string_value = "";
      }
      if (this.vtype == ValueType.Bool) {
        this.bool_value = false;
      }
      if (this.vtype == ValueType.Array) {
        this.array_value = [];
      }
    } else {
      this.vtype = ValueType.Null;
    }
  }

  override bool opEquals(Object _value) {
    if ((cast(IValue)_value) is null) {
      throw new Exception("Can not compare between incompatibility");
    }

    IValue value = cast(IValue)_value;

    if (this.vtype != value.vtype) {
      throw new Exception(
          "Can not compare between incompatibility vtype "
          ~ this.vtype.to!string ~ " and " ~ value.vtype.to!string);
    }

    final switch (this.vtype) with (ValueType) {
    case Long:
      return this.long_value == value.long_value;
    case String:
      return this.string_value == value.string_value;
    case Bool:
      return this.bool_value == value.bool_value;
    case Array:
      IValue[] value_array = value.getArray;
      foreach (i, IValue this_elem; this.getArray) {
        if (!this_elem.opEquals(value_array[i])) {
          return false;
        }
      }
      return true;
    case Null:
      throw new Exception("Can't compare with Null");
    }
  }

  override int opCmp(Object _value) {
    if ((cast(IValue)_value) is null) {
      throw new Exception("Can not compare between incompatibility");
    }

    IValue value = cast(IValue)_value;

    if (this.vtype != value.vtype) {
      throw new Exception(
          "Can not compare between incompatibility vtype "
          ~ this.vtype.to!string ~ " and " ~ value.vtype.to!string);
    }

    final switch (this.vtype) with (ValueType) {
    case Long:
      auto c = this.long_value, d = value.long_value;

      if (c == d) {
        return 0;
      }
      if (c < d) {
        return -1;
      }
      return 1;
    case String:
      auto c = this.string_value, d = value.string_value;

      if (c == d) {
        return 0;
      }
      if (c < d) {
        return -1;
      }
      return 1;
    case Bool:
      throw new Exception("Can't compare with Bool");
    case Array:
      throw new Exception("Can't compare with Array");
    case Null:
      throw new Exception("Can't compare with Null");
    }
  }

  IValue dup() {
    final switch (this.vtype) with (ValueType) {
    case Long:
      return new IValue(this.long_value);
    case String:
      return new IValue(this.string_value);
    case Bool:
      return new IValue(this.bool_value);
    case Array:
      return new IValue(this.array_value.dup);
    case Null:
      return new IValue;
    }
  }

  IValue opIndex(size_t idx) {
    enforce(this.vtype == ValueType.Array);

    if (!(idx < this.array_value.length)) {
      throw new Exception("Out of index of the Array, orded - " ~ idx.to!string
          ~ " but length of the array is " ~ this.array_value.length.to!string);
    }

    return this.array_value[idx];
  }

  void setArrayElement(size_t idx, IValue value) {
    enforce(this.vtype == ValueType.Array);

    if (!(idx < this.array_value.length)) {
      throw new Exception("Out of index of the Array, orded - " ~ idx.to!string
          ~ " but length of the array is " ~ this.array_value.length.to!string);
    }

    this.array_value[idx] = value;
  }

  OpcodeType type() {
    return OpcodeType.tIValue;
  }
}
