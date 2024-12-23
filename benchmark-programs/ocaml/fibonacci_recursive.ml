let rec fibonacci i =
  if i = 0 then
    i
  else if i = 1 then
    i
  else
    fibonacci (i - 1) + fibonacci (i - 2)

let main n =
  fibonacci n

let () =
  match Sys.argv with
  | [| _ |] -> print_endline (string_of_int (main 10))
  | [| _; s |] -> (
      match int_of_string_opt s with
      | Some n -> print_endline (string_of_int (main n))
      | None -> print_endline "argument is not a number")
  | _ -> print_endline "arguments are too many"
