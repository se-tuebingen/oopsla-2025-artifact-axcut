
fun iterate i f a =
  if i = 0 then
    a
  else
    iterate (i - 1) f (f a)

fun main n =
    iterate n (fn x => x + 1) 0;

case CommandLine.arguments () of
    [] => print (Int.toString (main 10) ^ "\n")
  | s :: [] => (
      case Int.fromString s of
          SOME n => print (Int.toString (main n) ^ "\n")
        | NONE => print "argument is not a number")
  | _ => print "arguments are too many";

