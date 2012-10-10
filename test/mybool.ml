type b = True | False

let () = 
  let x = True in
  match x with
  | True -> print_int 1
  | False -> print_int 0


