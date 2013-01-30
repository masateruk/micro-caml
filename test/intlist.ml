type intlist = Nil | Cons of int * intlist

let () = 
  let rec cons x xs = Cons(x, xs) in
  let rec length xs =
    match xs with
    | Nil -> 0
    | Cons(_, xs) -> 1 + length xs in
  let rec sum xs =
    match xs with
    | Nil -> 0
    | Cons(x, xs) -> x + sum xs in
  let e = Nil in
  let x = Cons(1, e) in
  let y = cons 2 x in
  (match e with
  | Nil -> print_int 0
  | _ -> print_int 1);
  (match x with
  | Nil -> print_int 0
  | Cons(y, _) -> print_int y);
  (match y with
  | Nil -> print_int 0
  | Cons(_, Cons(y, _)) -> print_int y
  | _ -> print_int 100);
  print_int (length y);
  print_int (sum y)






