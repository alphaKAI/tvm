import std.stdio, std.file;
import std.format, std.string;
import tvm.parser, tvm.vm, tvm.compiler;

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
        auto code = input.PARSER.buildAST.compileASTtoOpcode;
        //writeln("code : ", code);
        writeln("result : ", vm.execute(code));
      }
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
}

void fib_example() {
  /*function fib(n, a, b) {
      if (n < 0) {
        if (n % 2 == 0) {
          return (0-1) * fib(n * (0-1));
        } else {
          return (0-1) * fib(n * (0-1));
        }
      }
      if (n < 1) {
        return a;
      }
      if (n < 2) {
        return b;
      }
      return fib(n - 1, b, a + b);
    }
    println(fib(35, 0, 1));
    */
  /*function fib(n) {
      print("n : ");
      println(n);
      if (n <= 1) {
        return n;
      } else {
        return fib(n - 1) + fib(n - 2);
      }
      }
    println(fib(5));*/
  /*
  for (var i = 0; i < 11; i = i + 1) {
      print("fib(");
      print(i);
      print(") : ");
      println(fib(i));
    }*/
  /*
  
    function fib(n) {
      if (n <= 1) {
        return n;
      } else {
        return fib(n - 1) + fib(n - 2);
      }
    }
    println(fib(35));
  */
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
