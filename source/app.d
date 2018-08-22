import std.stdio;
import tvm.parser, tvm.vm;

void main() {
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
    },
    q{ 1; }, q{
      (1 + 2) * (4 / 2);
    }, q{
      func((1 + 2) * (4 / 2));
    }, q{
      1 + 2 * 3 / 4 + 1;
    }, q{
      1 + 1 + 1;
    }
  ];

  foreach (test_case; test_cases) {
    writeln("----------------------------");
    writeln("source: ", test_case);
    writeln(PARSER(test_case).buildAST);
  }

  auto code = PARSER("2 - 4;").buildAST.compileASTtoOpcode;
  VM vm = new VM;
  writeln("code : ", code);
  writeln("result : ", vm.execute(code));
}
