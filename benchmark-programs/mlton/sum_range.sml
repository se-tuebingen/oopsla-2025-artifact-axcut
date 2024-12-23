
fun range i n =
  if i < n then
    i :: range (i + 1) n
  else
    [];

fun sum xs =
  case xs of
    [] => 0
  | y :: ys => y + sum ys;

fun main n =
    sum (range 0 n);

case CommandLine.arguments () of
  [] => print (Int.toString (main 10) ^ "\n")
| s :: [] => (
    case Int.fromString s of
      SOME n => print (Int.toString (main n) ^ "\n")
    | NONE => print "argument is not a number")
| _ => print "arguments are too many";

