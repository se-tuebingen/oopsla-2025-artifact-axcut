
fun attempt i =
  if i = 0 then
    SOME i
  else
    case attempt (i - 1) of
        SOME x => SOME (x + 1)
      | NONE => NONE;

fun main n =
  case attempt n of
      SOME x => x
    | NONE => ~1;

case CommandLine.arguments () of
    [] => print (Int.toString (main 10) ^ "\n")
  | s :: [] => (
      case Int.fromString s of
          SOME n => print (Int.toString (main n) ^ "\n")
        | NONE => print "argument is not a number")
  | _ => print "arguments are too many";

