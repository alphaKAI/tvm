import std.stdio;
import tvm.parser, tvm.vm;

void main() {
  /*
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
*/
  /*
function fact(n, acc) {
      if (n <= 1) {
        return acc;
      } else {
        return fact(n - 1, acc * n);
      }
    }
    println(fact(4, 1));
*/

  auto code = PARSER(q{
    function fib(n, a, b) {
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
  }).buildAST.compileASTtoOpcode;
  VM vm = new VM;
  //writeln("code : ", code);
  writeln("result : ", vm.execute(code));
}
