module tvm.util;
import std.array;

class Stack(T) {
  T[] stack;

  this() {
  }

  @property T pop() {
    T t = stack[$ - 1];
    stack.length--;
    return t;
  }

  @property size_t length() {
    return stack.length;
  }

  @property void push(T value) {
    stack ~= value;
  }

  @property bool empty() {
    return stack.empty;
  }

  @property T front() {
    return pop;
  }

  @property void popFront() {
    pop;
  }

  @property void popAll() {
    foreach (_; this) {
    }
  }

  typeof(this) dup() {
    typeof(this) newStack = new typeof(this);
    newStack.stack = this.stack.dup;
    return newStack;
  }
}
