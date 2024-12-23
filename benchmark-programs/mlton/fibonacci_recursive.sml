
fun fibonacci i =
  if i = 0 then
    i
  else if i = 1 then
    i
  else
    fibonacci (i - 1) + fibonacci (i - 2);

fun main n =
    fibonacci n;

case CommandLine.arguments () of
    [] => print (Int.toString (main 10) ^ "\n")
  | s :: [] => (
      case Int.fromString s of
          SOME n => print (Int.toString (main n) ^ "\n")
        | NONE => print "argument is not a number")
  | _ => print "arguments are too many";

