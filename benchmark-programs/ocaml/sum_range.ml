let rec range i n =
  if i < n then
    i :: range (i + 1) n
  else
    []

let rec sum xs =
  match xs with
  | [] -> 0
  | y :: ys -> y + sum ys

let main n =
  sum (range 0 n)

let () =
  match Sys.argv with
  | [| _ |] -> print_endline (string_of_int (main 10))
  | [| _; s |] -> (
      match int_of_string_opt s with
      | Some n -> print_endline (string_of_int (main n))
      | None -> print_endline "argument is not a number")
  | _ -> print_endline "arguments are too many"
