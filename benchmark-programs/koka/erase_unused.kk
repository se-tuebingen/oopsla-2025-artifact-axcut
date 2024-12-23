module erase_unused

import std/num/int64
import std/os/env

fun replicate(v : int64, n : int64, a) {
  if n == 0.int64 then
    a
  else
    replicate(v, n - 1.int64, Cons(v, a))
}

fun useless(i : int64, n : int64, _) {
  if i < n then
    useless(i + 1.int64, n, replicate(0.int64, i, Nil))
  else
    i
}

fun run-example(n : int64) {
  useless(0.int64, n, Nil)
}

pub fun main() {
  match get-args()
    Nil -> print(run-example(10.int64).show ++ "\n")
    Cons(s, Nil) -> match parse-int(s)
      Just(n) -> print(run-example(n.int64).show ++ "\n")
      Nothing -> print("argument is not a number")
    _ -> print("arguments are too many")
}
