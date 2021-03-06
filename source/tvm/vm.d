module tvm.vm;
import tvm.parser, tvm.value, tvm.util, tvm.opcode;
import std.typecons, std.algorithm, std.format, std.conv;
import std.stdio;

alias HasPtrResult = Tuple!(IValue*, "iv", VariableStore, "that");

class VariableStore {
  private VariableStore superStore;
  private bool hasSuper;
  private string[] protecteds;
  private IValue[string] store;

  this() {
  }

  this(VariableStore vs) {
    this.superStore = vs;
    this.hasSuper = true;
  }

  /*
    -親が存在する
      - 書き換えて良い場合→親のstoreを書き換える
      - 書き換えては行けない場合→子のstoreを書き換える
    - 親が存在しない
      - 子のstoreを操作する
   */

  /**
    * superクラスのstoreにkeyに対応するIValueが存在するかを判定する
    */
  bool superHas(string key) {
    if (this.hasSuper) {
      return this.superStore.has(key);
    } else {
      return false;
    }
  }

  /**
    * 現在のインスタンス，もしくは親に，keyに対応するIValueが存在するかを判定する
    */
  bool has(string key) {
    bool ret = key in this.store ? true : false;

    if (this.hasSuper) {
      ret |= this.superStore.has(key);
    }

    return ret;
  }

  /**
    * superクラスのstoreにkeyに対応するIValueが存在するかを判定する
    */
  HasPtrResult superHas_ptr(string key) {
    if (this.hasSuper) {
      return this.superStore.has_ptr(key);
    } else {
      return HasPtrResult.init;
    }
  }

  /**
    * 現在のインスタンス，もしくは親に，keyに対応するIValueが存在するかを判定する
    */
  HasPtrResult has_ptr(string key) {
    HasPtrResult ret = tuple(key in this.store, this);

    if (ret.iv is null && this.hasSuper) {
      ret = this.superStore.has_ptr(key);
    }

    return ret;
  }

  enum HIGH_PERFORMANCE = true;

  static if (HIGH_PERFORMANCE) {
    /**
    * keyに対応するIValueを返す．
    */
    IValue get(string key) {
      // 親が存在するかを判定する
      if (this.hasSuper) {
        // 保護されている場合，このインスタンスのstoreから参照する
        if (this.protecteds.canFind(key)) {
          return this.store[key];
        } else {
          // 親がkeyを持っているかみる
          auto ptr = this.superHas_ptr(key);
          if (ptr.iv !is null) {
            // 持っている場合，親から参照する
            return *ptr.iv;
          } else {
            // 持っていない場合，現在のインスタンスから参照する．
            return this.store[key];
          }
        }
      } else {
        // 親が存在しないので，現在のインスタンスから参照する．
        return this.store[key];
      }
    }
  } else {
    /**
    * keyに対応するIValueを返す．
    */
    IValue get(string key) {
      // 親が存在するかを判定する
      if (this.hasSuper) {
        // 保護されている場合，このインスタンスのstoreから参照する
        if (this.protecteds.canFind(key)) {
          return this.store[key];
        } else {
          // 親がkeyを持っているかみる
          if (this.superHas(key)) {
            // 持っている場合，親から参照する
            return this.superStore.get(key);
          } else {
            // 持っていない場合，現在のインスタンスから参照する．
            return this.store[key];
          }
        }
      } else {
        // 親が存在しないので，現在のインスタンスから参照する．
        return this.store[key];
      }
    }
  }

  /**
    * 変数を定義する．
    * 定義したスコープで保護するようにする．(親の変数を触らなくする)
    */
  void def(string key, IValue value) {
    // 親がいるか判定する．
    if (this.hasSuper) {
      // 親がいる場合，親がkeyを持っているかを判定する
      if (this.superHas(key)) {
        // 持っている場合，保護対象として，protectedsに追加し，保存する．
        this.protecteds ~= key;
        this.store[key] = value;
      } else {
        // 持っていない場合，何もせずに現在のインスタンスに保存する
        this.store[key] = value;
      }
    } else {
      // 親がいない場合は現在のインスタンスのstoreに保存する．
      this.store[key] = value;
    }
  }

  static if (HIGH_PERFORMANCE) {
    /**
    * 変数に値を代入する
    */
    void set(string key, IValue value) {
      // 親が存在するかの確認
      if (this.hasSuper) { //存在する
        // 保護されている(つまり，現在のスコープで定義されている場合)場合は親を書き換えてはいけないので
        // このインスタンスのstoreを書き換える．
        if (this.protecteds.canFind(key)) {
          this.store[key] = value;
        } else {
          // 保護されていない場合，親がkeyを持っているかをみる．
          auto ptr = this.superHas_ptr(key);
          if (ptr.iv !is null) {
            // 親がkeyを持っている場合，親にsetさせる．
            ptr.that.set(key, value);
          } else {
            // 親がkeyを持っていない場合，自分のstoreを書き換える．
            this.store[key] = value;
          }
        }
      } else {
        // 存在しないなら現在のstoreを変更して良い
        this.store[key] = value;
      }
    }
  } else {
    /**
    * 変数に値を代入する
    */
    void set(string key, IValue value) {
      // 親が存在するかの確認
      if (this.hasSuper) { //存在する
        // 保護されている(つまり，現在のスコープで定義されている場合)場合は親を書き換えてはいけないので
        // このインスタンスのstoreを書き換える．
        if (this.protecteds.canFind(key)) {
          this.store[key] = value;
        } else {
          // 保護されていない場合，親がkeyを持っているかをみる．
          if (this.superHas(key)) {
            // 親がkeyを持っている場合，親にsetさせる．
            this.superStore.set(key, value);
          } else {
            // 親がkeyを持っていない場合，自分のstoreを書き換える．
            this.store[key] = value;
          }
        }
      } else {
        // 存在しないなら現在のstoreを変更して良い
        this.store[key] = value;
      }
    }
  }
}

class Env {
  VariableStore vs;

  this() {
    this.vs = new VariableStore;
  }

  this(VariableStore vs) {
    this.vs = new VariableStore(vs);
  }

  Env dup() {
    return new Env(this.vs);
  }

  /**
    * Proxy of this.vs.get
    */
  IValue get(string key) {
    return this.vs.get(key);
  }

  /**
    * Proxy of this.vs.def
    */
  void def(string key, IValue value) {
    this.vs.def(key, value);
  }

  /**
    * Proxy of this.vs.set
    */
  void set(string key, IValue value) {
    this.vs.set(key, value);
  }

  /**
    * Proxy of this.vs.has
    */
  bool has(string key) {
    return this.vs.has_ptr(key).iv !is null;
  }

  /**
    * Proxy of this.vs.has
    */
  HasPtrResult has_ptr(string key) {
    return this.vs.has_ptr(key);
  }
}

class VMException : Exception {
  this(string msg) {
    super("VMException - " ~ msg);
  }
}

class VM {
  Env env;
  Stack!IValue stack;

  this() {
    this.env = new Env;
    this.stack = new Stack!IValue;

    this.env.def("print", new IValue(new VMFunction("print", [opPrint], env)));
    this.env.def("println", new IValue(new VMFunction("println", [opPrintln], env)));
  }

  IValue execute(Opcode[] code) {
    IValue stackPeekTop() {
      if (!stack.empty) {
        return stack.front;
      } else {
        return null;
      }
    }

    for (size_t pc; pc < code.length; pc++) {
      Opcode op = code[pc];
      //writeln("op : ", op.type);
      final switch (op.type) with (OpcodeType) {
      case tOpVariableDeclareOnlySymbol:
        auto symbol = cast(IValue)code[pc++ + 1];
        this.env.def(symbol.getString, new IValue);
        break;
      case tOpVariableDeclareWithAssign:
        auto symbol = cast(IValue)code[pc++ + 1];
        auto v = stack.pop;
        this.env.def(symbol.getString, v);
        break;
      case tOpAssignExpression:
        auto symbol = cast(IValue)code[pc++ + 1];
        auto v = stack.pop;
        this.env.set(symbol.getString, v);
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
        IValue a = stack.pop, b = stack.pop;
        stack.push(a + b);
        break;
      case tOpSub:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a - b);
        break;
      case tOpMul:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a * b);
        break;
      case tOpDiv:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a / b);
        break;
      case tOpMod:
        IValue a = stack.pop, b = stack.pop;
        stack.push(a % b);
        break;
      case tOpReturn:
        return stackPeekTop;
      case tOpGetVariable:
        auto v = cast(IValue)code[pc++ + 1];
        assert(v !is null, "Execute Error on tOpGetVariable");
        auto ptr = this.env.has_ptr(v.getString);
        if (ptr.iv !is null) {
          stack.push(*ptr.iv);
        } else {
          throw new Exception("No such a variable %s".format(v.getString));
        }
        break;
      case tOpSetVariablePop:
        auto dst = cast(IValue)code[pc++ + 1];
        auto v = stack.pop;
        this.env.set(dst.getString, v);
        break;
      case tOpCall:
        auto func = cast(IValue)code[pc++ + 1];
        string fname = func.getString;
        Env cpyEnv = this.env;
        this.env = this.env.get(fname).getFunction.func_env.dup;
        this.execute(cpyEnv.get(fname).getFunction.func_body);
        this.env = cpyEnv;
        break;
      case tOpNop:
        break;
      case tOpFunctionDeclare:
        auto symbol = cast(IValue)code[pc++ + 1];
        string func_name = symbol.getString;
        auto op_blocks_length = cast(IValue)code[pc++ + 1];
        Opcode[] func_body;
        foreach (_; 0 .. op_blocks_length.getLong) {
          func_body ~= code[pc++ + 1];
        }
        this.env.def(func_name, new IValue(new VMFunction(func_name, func_body, env.dup)));
        break;
      case tOpEqualExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a == b));
        break;
      case tOpNotEqualExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a != b));
        break;
      case tOpLtExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a < b));
        break;
      case tOpLteExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a <= b));
        break;
      case tOpGtExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a > b));
        break;
      case tOpGteExpression:
        IValue a = stack.pop, b = stack.pop;
        stack.push(new IValue(a >= b));
        break;
      case tOpAndExpression:
        bool a = stack.pop.getBool, b = stack.pop.getBool;
        stack.push(new IValue(a && b));
        break;
      case tOpOrExpression:
        bool a = stack.pop.getBool, b = stack.pop.getBool;
        stack.push(new IValue(a || b));
        break;
      case tOpXorExpression:
        throw new Error("Not implemented <%s>".format(op.type));
      case tOpPrint:
        auto v = stack.pop;
        if (v.vtype == ValueType.String) {
          write(v.getString);
        } else {
          write(v);
        }
        break;
      case tOpPrintln:
        auto v = stack.pop;
        if (v.vtype == ValueType.String) {
          writeln(v.getString);
        } else {
          writeln(v);
        }
        break;
      case tOpJumpRel:
        auto v = cast(IValue)code[pc++ + 1];
        pc += v.getLong;
        break;
      case tOpJumpAbs:
        auto v = cast(IValue)code[pc++ + 1];
        pc = v.getLong;
        break;
      case tOpIFStatement:
        auto cond = stack.pop;
        bool condResult;
        final switch (cond.vtype) with (ValueType) {
        case Long:
          condResult = cond.getLong != 0;
          break;
        case Bool:
          condResult = cond.getBool;
          break;
        case String:
          throw new Exception("Execute Error Invalid Condition <string>");
        case Array:
          throw new Exception("Execute Error Invalid Condition <array>");
        case Function:
          throw new Exception("Execute Error Invalid Condition <function>");
        case Null:
          condResult = false;
          break;
        }

        auto trueBlockLength = (cast(IValue)code[pc++ + 1]).getLong;

        if (condResult) {
          break;
        } else {
          pc += trueBlockLength;
        }
        break;
      case tOpSetArrayElement:
        auto variable = (cast(IValue)code[pc++ + 1]).getString;
        auto idx = stack.pop().getLong;
        auto val = stack.pop;
        env.get(variable).setArrayElement(idx, val);
        break;
      case tOpGetArrayElement:
        auto variable = (cast(IValue)code[pc++ + 1]).getString;
        auto idx = stack.pop().getLong;
        stack.push(env.get(variable)[idx]);
        break;
      case tOpMakeArray:
        long array_size = (cast(IValue)code[pc++ + 1]).getLong;
        IValue[] array;
        array.length = array_size;
        foreach_reverse (i; 0 .. array_size) {
          array[i] = stack.pop();
        }
        stack.push(new IValue(array));
        break;
      case tIValue:
        throw new Error("IValue should not peek directly");
      case tOpAssert:
        auto msg = stack.pop().getString;
        auto result = stack.pop().getBool;
        if (!result) {
          throw new VMException(msg);
        }
      }
    }
    return stackPeekTop();
  }
}
