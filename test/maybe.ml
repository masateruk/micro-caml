type 'a maybe = Nothing | Just of 'a

let () =
  let rec id x = x in
  let rec foo x = 
    match x with
    | Nothing -> Nothing
    | Just y -> Just y in
  let m = id Nothing in
  let n = id (Just 10) in
  let b = id (Just true) in
  (match n with
  | Nothing -> print_int 0
  | Just i -> print_int i);
  match b with
  | Nothing -> print_int 0
  | Just true -> print_int 1
  | Just false -> print_int 0
