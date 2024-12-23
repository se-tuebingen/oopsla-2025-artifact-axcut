
datatype Tree =
    LEAF of int
  | NODE of Tree * Tree;

fun create i n =
  if i < n then
    let
      val t = create (i + 1) n;
    in
      NODE (t, t)
    end
  else
    LEAF n;

fun lookup t =
  case t of
    LEAF v => v
  | NODE (l, _) => lookup l;

fun main n =
    lookup (create 0 n);

case CommandLine.arguments () of
    [] => print (Int.toString (main 10) ^ "\n")
  | s :: [] => (
      case Int.fromString s of
          SOME n => print (Int.toString (main n) ^ "\n")
        | NONE => print "argument is not a number")
  | _ => print "arguments are too many";

