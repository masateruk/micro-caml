type 'a maybe = Nothing | Just of 'a

let () =
  let m = Nothing in
  let n = Just 10 in
  let b = Just true in
  (match n with
  | Nothing -> print_int 0
  | Just i -> print_int i);
  match b with
  | Nothing -> print_int 0
  | Just true -> print_int 1
  | Just false -> print_int 0

    



