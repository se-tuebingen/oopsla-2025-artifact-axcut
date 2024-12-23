module fibonacci_recursive

import std/num/int64
import std/os/env

fun fibonacci(i : int64) : <div> int64 {
  if i == 0.int64 then
    i
  else if i == 1.int64 then
    i
  else
    fibonacci(i - 1.int64) + fibonacci(i - 2.int64)
}

fun run-example(n : int64) {
  fibonacci(n)
}

pub fun main() {
  match get-args()
    Nil -> print(run-example(10.int64).show ++ "\n")
    Cons(s, Nil) -> match parse-int(s)
      Just(n) -> print(run-example(n.int64).show ++ "\n")
      Nothing -> print("argument is not a number")
    _ -> print("arguments are too many")
}
