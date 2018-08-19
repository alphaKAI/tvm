import std.stdio;
import tvm.parser;

void main() {
	string[] test_cases = [
		"var foo;", `var foo = "value";`, "var foo = variable;",
		`function funcName(a, b) {
      a = b;
      a + b;
      a - b;
      a * b;
      a / d;
      a % d;
      return a;
    }`, "abc()", "def(g, h)", q{
      var a = 1;
      var b = 2;
      function add(a, b) { return a + b; }
      add(a, b);
    }
	];

	foreach (test_case; test_cases) {
		writeln("----------------------------");
		writeln("source: ", test_case);
		writeln(JS(test_case).buildAST);
	}
}
