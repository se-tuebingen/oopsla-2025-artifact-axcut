let rec factorial a i =
  if i = 0 then
    a
  else
    factorial ((i * a) mod 1000000007) (i - 1)

let main n =
  factorial 1 n

let () =
  match Sys.argv with
  | [| _ |] -> print_endline (string_of_int (main 10))
  | [| _; s |] -> (
      match int_of_string_opt s with
      | Some n -> print_endline (string_of_int (main n))
      | None -> print_endline "argument is not a number")
  | _ -> print_endline "arguments are too many"
