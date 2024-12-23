let rec replicate v n a =
  if n = 0 then a
  else replicate v (n - 1) (v :: a)

let rec useless i n _ =
  if i < n then useless (i + 1) n (replicate 0 i [])
  else i

let main n =
  useless 0 n []

let () =
  match Sys.argv with
  | [| _ |] -> print_endline (string_of_int (main 10))
  | [| _; s |] -> (
      match int_of_string_opt s with
      | Some n -> print_endline (string_of_int (main n))
      | None -> print_endline "argument is not a number")
  | _ -> print_endline "arguments are too many"
