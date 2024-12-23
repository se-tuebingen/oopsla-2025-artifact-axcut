
fun replicate v n a =
  if n = 0
    then a
    else replicate v (n - 1) (v :: a)

fun useless i n _ =
  if i < n
    then useless (i + 1) n (replicate 0 i [])
    else i

fun main n =
    useless 0 n [];

case CommandLine.arguments () of
    [] => print (Int.toString (main 10) ^ "\n")
  | s :: [] => (
      case Int.fromString s of
          SOME n => print (Int.toString (main n) ^ "\n")
        | NONE => print "argument is not a number")
  | _ => print "arguments are too many";

