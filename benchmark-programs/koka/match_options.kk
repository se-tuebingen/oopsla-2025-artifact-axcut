module match_options

import std/num/int64
import std/os/env

fun attempt(i : int64) : <div> maybe<int64> {
  if i == 0.int64 then
    Just(i)
  else
    match attempt(i - 1.int64)
      Just(x) -> Just(x + 1.int64)
      Nothing -> Nothing
}

fun run-example(n : int64) {
  match attempt(n)
    Just(x) -> x
    Nothing -> -1.int64
}

pub fun main() {
  match get-args()
    Nil -> print(run-example(10.int64).show ++ "\n")
    Cons(s, Nil) -> match parse-int(s)
      Just(n) -> print(run-example(n.int64).show ++ "\n")
      Nothing -> print("argument is not a number")
    _ -> print("arguments are too many")
}
