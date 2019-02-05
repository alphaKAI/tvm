import std.stdio, std.file;
import std.format, std.string;
import tvm.parser, tvm.vm, tvm.compiler, tvm.serializer, tvm.deserializer;

void main(string[] args) {
  args = args[1 .. $];
  if (args.length == 0) {
    auto code = PARSER(q{
      println("Hello, world!");
    }).buildAST.compileASTtoOpcode;
    VM vm = new VM;
    writeln("code : ", code);
    writeln("result : ", vm.execute(code));
  } else {
    VM vm = new VM;
    if (args[0] == "-r" || args[0] == "--repl") {
      for (;;) {
        write("=> ");
        string input = readln.chomp;
        if (input == "exit") {
          break;
        }
        //auto code = input.PARSER.buildAST.compileASTtoOpcode;
        auto parsed = input.PARSER;
        auto ast = parsed.buildAST;
        auto code = ast.compileASTtoOpcode;
        writeln("parsed : ", parsed);
        writeln("ast : ", ast);
        writeln("code : ", code);
        writeln("result : ", vm.execute(code));
        writeln("serialize:");
        auto serialized = serialize(code);
        writeln("serialized : ", serialized);
        auto deserialized = deserialize(serialized);
        writeln("deserialized : ", deserialized);
        writeln("code == deserialized : ", code == deserialized);
      }
    } else if (args[0] == "-c" || args[0] == "--compile") {
      compileFile(args[1]);
    } else if (args[0] == "-e" || args[0] == "--execute") {
      executeBinaryFile(args[1]);
    } else if (args[0] == "-t" || args[0] == "--test") {
      serializeTestFile(args[1]);
    } else {
      executeFile(args[0]);
    }
  }
}

void executeFile(string file_path) {
  auto code = readText(file_path).PARSER.buildAST.compileASTtoOpcode;
  VM vm = new VM;
  writeln("code : ", code);
  writeln("result : ", vm.execute(code));
  /*
  auto input = readText(file_path);
  auto parsed = input.PARSER;
  auto ast = parsed.buildAST;
  auto code = ast.compileASTtoOpcode;
  writeln("parsed : ", parsed);
  writeln("ast : ", ast);
  writeln("code : ", code);
  VM vm = new VM;
  writeln("result : ", vm.execute(code));
  */
}

void serializeTestFile(string file_path) {
  auto input = readText(file_path);
  auto parsed = input.PARSER;
  auto ast = parsed.buildAST;
  auto code = ast.compileASTtoOpcode;
  writeln("parsed : ", parsed);
  writeln("ast : ", ast);
  writeln("code : ", code);
  VM vm = new VM;
  writeln("result : ", vm.execute(code));
  writeln("serialize:");
  auto serialized = serialize(code);
  writeln("serialized : ", serialized);
  auto deserialized = deserialize(serialized);
  writeln("deserialized : ", deserialized);
  writeln("code == deserialized : ", code == deserialized);

  saveToFile(code, "compiled");
  auto code2 = readFromFile("compiled");
  writeln("code  : ", code);
  writeln("code2 : ", code2);
}

void compileFile(string file_path) {
  auto input = readText(file_path);
  auto parsed = input.PARSER;
  auto ast = parsed.buildAST;
  auto code = ast.compileASTtoOpcode;

  import std.path;

  string out_path = baseName(file_path) ~ ".compiled";

  writeln("Compile to file");
  writeln("code : ", code);
  writeln("out_path : ", out_path);

  saveToFile(code, out_path);
}

void executeBinaryFile(string file_path) {
  writeln("Execute Binery File : ", file_path);
  auto code = readFromFile(file_path);
  writeln("code : ", code);
  VM vm = new VM;
  vm.execute(code);
}

void parse_test() {
  string[] test_cases = [
    "var foo;", `var foo = "value";`, "var foo = variable;", `function funcName(a, b) {
      a = b;
      a + b;
      a - b;
      a * b;
      a / d;
      a % d;
      return a;
    }`,
    "abc();", "def(g, h);", q{
      var a = 1;
      var b = 2;
      function add(a, b) { return a + b; }
      add(a, b);
    }, "true;", q{
      if (1 == a) {
        abc();
      }
    }, q{
      if (a == b) {
        return abc();
      } else {
        return def();
      }
    }, "a || b;", q{
      function func() {
        return 0;
      }
    }, q{
      function func() {
        return 1 || b;
      }
    }, q{ 1; },
    q{
      (1 + 2) * (4 / 2);
    }, q{
      func((1 + 2) * (4 / 2));
    }, q{
      1 + 2 * 3 / 4 + 1;
    }, q{
      1 + 1 + 1;
    }, q{
      [];
    }, q{
      [1, 2, 3];
    }, q{
      [1, "abc", [[], 1, 2, 3]];
    }
  ];

  foreach (test_case; test_cases) {
    writeln("----------------------------");
    writeln("source: ", test_case);
    writeln(PARSER(test_case).buildAST);
  }
}

void exec_test() {
  import tvm.value;

  bool test(string src, IValue v) {
    auto code = src.PARSER.buildAST.compileASTtoOpcode;
    VM vm = new VM;
    IValue ret = vm.execute(code);
    return ret == v;
  }

  import std.typecons;

  //dfmt off
  Tuple!(string, IValue)[] vm_test_cases = [
    tuple("1 + 1;", new IValue(2)),
    tuple("2 - 1;", new IValue(1L)),
    tuple("1 - 2;", new IValue(-1)),
    tuple("1 * 2;", new IValue(2)),
    tuple("3 * 4;", new IValue(12)),
    tuple("4 / 2;", new IValue(2)),
    tuple("2 / 4;", new IValue(0L)),
    tuple("(1 + 2) * (4 / 2);", new IValue(6)),
    tuple(q{
      function func(a, b) {
        return a + b;
      }
      func(1, 3);
    }, new IValue(4)),
    tuple(q{
      function f(a) {
        if (a == 1) {
          return true;
        } else {
          return false;
        }
      }
      f(1);
    }, new IValue(true)),
    tuple(q{
      function f(a) {
        if (a == 1) {
          return true;
        }
        return false;
      }
      f(1);
    }, new IValue(true)),
    tuple(q{
      function f(a) {
        if (a == 1) {
          return true;
        } else {
          return false;
        }
      }
      f(2);
    }, new IValue(false)),
    tuple(q{
      function f(a) {
        if (a == 1) {
          return true;
        } 
        return false;
      }
      f(2);
    }, new IValue(false)),
    tuple(q{
      function f(a) {
        return a;
      }
      1 + f(3);
    }, new IValue(4)),
    tuple(q{
      function fact(n, acc) {
        if (n <= 1) {
          return acc;
        } else {
          return fact(n - 1, acc * n);
        }
      }
      fact(4, 1);
    }, new IValue(24)),
    tuple(q{
      var sum = 0;
      for(var a = 1; a < 11; a = a + 1) {
        sum = sum + a;
      }
      sum;
    }, new IValue(55)),
    tuple(q{
      function mult(a, b) {
        return a * b;
      }
      function func(n) {
        if (n <= 1) {
          return 1;
        } else {
          return mult(n, func(n - 1));
        }
      }
      func(10);
    }, new IValue(3628800))
  ];
  foreach (test_case; vm_test_cases) {
    string src = test_case[0];
    IValue v = test_case[1];
    auto val = test(src, v);
    assert(val, "Test for %s, result: %s".format(src, val));
  }
  //dfmt on
}
