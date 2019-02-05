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

R[] numeric_to_lower(T, R)(T v) {
  R[] rets;
  size_t T_size = T.sizeof;
  size_t R_size = R.sizeof;
  rets.length = T_size / R_size;

  foreach (i; 0 .. (T_size / R_size)) {
    rets[i] = v >> (R_size * i) & R.max;
  }

  return rets;
}

R lowers_to_numeric(T, R)(T[] inputs) {
  R v;
  size_t T_size = T.sizeof;
  size_t R_size = R.sizeof;

  foreach (i; 0 .. (R_size / T_size)) {
    v |= inputs[i] << (T_size * i);
  }

  return v;
}

alias long_to_bytes = numeric_to_lower!(long, ubyte);
alias bytes_to_long = lowers_to_numeric!(ubyte, long);
