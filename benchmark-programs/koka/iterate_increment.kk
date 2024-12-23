module iterate_increment

import std/num/int64
import std/os/env

fun iterate(i : int64, f, a) {
  if i == 0.int64 then
    a
  else
    iterate(i - 1.int64, f, f(a))
}

fun run-example(n : int64) {
  iterate(n, fn(x) x + 1.int64, 0.int64)
}

pub fun main() {
  match get-args()
    Nil -> print(run-example(10.int64).show ++ "\n")
    Cons(s, Nil) -> match parse-int(s)
      Just(n) -> print(run-example(n.int64).show ++ "\n")
      Nothing -> print("argument is not a number")
    _ -> print("arguments are too many")
}
