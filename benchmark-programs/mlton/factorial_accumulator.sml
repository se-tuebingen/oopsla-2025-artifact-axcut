
fun factorial a i =
  if i = 0 then
    a
  else
    factorial ((i * a) mod 1000000007) (i - 1);

fun main n =
    factorial 1 n;

case CommandLine.arguments () of
    [] => print (Int.toString (main 10) ^ "\n")
  | s :: [] => (
      case Int.fromString s of
          SOME n => print (Int.toString (main n) ^ "\n")
        | NONE => print "argument is not a number")
  | _ => print "arguments are too many";

