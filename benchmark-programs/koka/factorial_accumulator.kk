module factorial_accumulator

import std/num/int64
import std/os/env

fun factorial(a : int64, i : int64) : <div> int64 {
  if i == 0.int64 then a
  else factorial((i * a) % 1000000007.int64, i - 1.int64)
}

fun run-example(n : int64) : <div> int64 {
  factorial(1.int64, n)
}

pub fun main() {
  match get-args()
    Nil -> print(run-example(20.int64).show ++ "\n")
    Cons(s, Nil) -> match parse-int(s)
      Just(n) -> print(run-example(n.int64).show ++ "\n")
      Nothing -> print("argument is not a number")
    _ -> print("arguments are too many")
}
