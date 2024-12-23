module lookup_tree

import std/num/int64
import std/os/env

type tree {
  Leaf(n : int64)
  Node(l : tree, r : tree)
}

fun create(i : int64, n : int64): <div> tree {
  if i < n then
    val t = create(i + 1.int64, n)
    Node(t, t)
  else
    Leaf(n)
}

fun lookup(t : tree) {
  match t
    Leaf(v) -> v
    Node(l, _) -> lookup(l)
}

fun run-example(n : int64) {
  lookup(create(0.int64, n))
}

pub fun main() {
  match get-args()
    Nil -> print(run-example(10.int64).show ++ "\n")
    Cons(s, Nil) -> match parse-int(s)
      Just(n) -> print(run-example(n.int64).show ++ "\n")
      Nothing -> print("argument is not a number")
    _ -> print("arguments are too many")
}
