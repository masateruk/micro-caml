type 'a list = Nil | Cons of 'a * 'a list

let () = 
  let e = Nil in
  let xs = Cons(11, e) in
  let ys = Cons(12, xs) in
  let bs = Cons(true, e) in
  (match xs with
  | Nil -> print_int 0
  | Cons(x, _) -> print_int x);
  (match ys with
  | Nil -> print_int 0
  | Cons(y, Cons(z, _)) -> print_int y; print_int z
  | _ -> print_int 256);
  (match bs with
  | Nil -> print_int 0
  | Cons(b, _) -> if b then (print_int 1) else (print_int 0));
  ()
