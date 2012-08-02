type pos = { x : int; y : int }
type single = { z : int }
type nb = MyInt of int | MyBool of bool
type pair = IntPair of int * int | BoolPair of bool * bool

let () =
  let a = 1 in
  let b = true in
  let n = false in
  let c = 10 in
  let p = { x = 0; y = 10 } in
  let s = { z = 0 } in
  let ip = IntPair(100, 200) in
  let x = match ip with
  | IntPair(n, x2) -> n
  | BoolPair(b1, false) -> 0
  | v -> (-1) in
  print_int x;
  match x with
  | 0 -> print_int 0
  | x -> print_int x
