let rec iterate i f a =
  if i = 0 then
    a
  else
    iterate (i - 1) f (f a)

let main n =
  iterate n (fun x -> x + 1) 0

let () =
  match Sys.argv with
  | [| _ |] -> print_endline (string_of_int (main 10))
  | [| _; s |] -> (
      match int_of_string_opt s with
      | Some n -> print_endline (string_of_int (main n))
      | None -> print_endline "argument is not a number")
  | _ -> print_endline "arguments are too many"
