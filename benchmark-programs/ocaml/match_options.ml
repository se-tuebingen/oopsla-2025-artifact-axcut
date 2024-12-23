let rec attempt i =
  if i = 0 then
    Some i
  else
    match attempt (i - 1) with
    | Some x -> Some (x + 1)
    | None -> None

let main n =
  match attempt n with
  | Some x -> x
  | None -> -1

let () =
  match Sys.argv with
  | [| _ |] -> print_endline (string_of_int (main 10))
  | [| _; s |] -> (
      match int_of_string_opt s with
      | Some n -> print_endline (string_of_int (main n))
      | None -> print_endline "argument is not a number")
  | _ -> print_endline "arguments are too many"
