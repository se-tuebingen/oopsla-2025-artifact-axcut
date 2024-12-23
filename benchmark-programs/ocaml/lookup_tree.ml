type tree =
  | Leaf of int
  | Node of tree * tree

let rec create i n =
  if i < n then
    let t = create (i + 1) n in
    Node (t, t)
  else
    Leaf n

let rec lookup t =
  match t with
  | Leaf v -> v
  | Node (l, _) -> lookup l

let main n =
  lookup (create 0 n)

let () =
  match Sys.argv with
  | [| _ |] -> print_endline (string_of_int (main 10))
  | [| _; s |] -> (
      match int_of_string_opt s with
      | Some n -> print_endline (string_of_int (main n))
      | None -> print_endline "argument is not a number")
  | _ -> print_endline "arguments are too many"
