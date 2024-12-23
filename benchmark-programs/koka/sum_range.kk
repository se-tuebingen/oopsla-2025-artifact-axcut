module sum_range

import std/num/int64
import std/os/env

fun range(i : int64, n : int64) : <div> list<int64> {
  if i < n then
    Cons(i, range(i + 1.int64, n))
  else
    Nil
}

// sum is already defined in the stdlib
fun sum'(xs : list<int64>): int64 {
  match xs
    Nil -> 0.int64
    Cons(y, ys) -> y + sum'(ys)
}

fun run-example(n : int64) {
  sum'(range(0.int64, n))
}

pub fun main() {
  match get-args()
    Nil -> print(run-example(10.int64).show ++ "\n")
    Cons(s, Nil) -> match parse-int(s)
      Just(n) -> print(run-example(n.int64).show ++ "\n")
      Nothing -> print("argument is not a number")
    _ -> print("arguments are too many")
}
