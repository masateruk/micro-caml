type intlist = INil | ICons of int * intlist

let () = 
  let rec cons x xs = ICons(x, xs) in
  let rec length xs =
    match xs with
    | INil -> 0
    | ICons(_, xs) -> 1 + length xs in
  let rec sum xs =
    match xs with
    | INil -> 0
    | ICons(x, xs) -> x + sum xs in
  let e = INil in
  let x = ICons(1, e) in
  let y = cons 2 x in
  (match e with
  | INil -> print_int 0
  | _ -> print_int 1);
  (match x with
  | INil -> print_int 0
  | ICons(y, _) -> print_int y);
  (match y with
  | INil -> print_int 0
  | ICons(_, ICons(y, _)) -> print_int y
  | _ -> print_int 100);
  print_int (length y);
  print_int (sum y)






