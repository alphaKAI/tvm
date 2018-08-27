module tvm.util;
import std.array;

class Stack(T) {
  T[] stack;
  size_t capacity;
  size_t count;

  this() {
    capacity = 1024;
    stack.length = capacity;
  }

  @property T pop() {
    T t = stack[count-- - 1];
    return t;
  }

  @property size_t length() {
    return stack.length;
  }

  @property void push(T value) {
    if (count + 1 == capacity) {
      capacity += 1024;
      stack.length = capacity;
    }
    stack[count++] = value;
  }

  @property bool empty() {
    return count == 0;
  }

  @property T front() {
    return stack[count - 1];
  }

  @property void popFront() {
    count--;
  }

  @property void popAll() {
    foreach (_; this) {
    }
  }
}
