type pos = { x : int; y : int }
type single = { z : int }
type b = True | False
type nb = MyInt of int | MyBool of bool
type pair = IntPair of int * int | BoolPair of bool * bool

let () =
  let rec eq x y = x = y in
  let a = 1 in
  let b = true in
  let b1 = True in
  let b2 = True in
  let n = false in
  let c = 10 in
  let p = { x = 0; y = 10 } in
  let s = { z = 0 } in
  let ip = IntPair(100, 200) in
  if (b1 = b2) then print_int 11 else print_int 22;
  if (eq b1 b2) then print_int 11 else print_int 22;
  let x = match ip with
  | IntPair(n, x2) -> n
  | BoolPair(b1, false) -> 0
  | v -> (-1) in
  print_int x;
  let b = True in
  (match b with
  | True -> print_int 1
  | _ -> print_int 0);
  match x with
  | 0 -> print_int 0
  | x -> print_int x
