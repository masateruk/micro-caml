type 'a list = Nil | Cons of 'a * 'a list
    
let () =
  let rec iter f xs =
    match xs with
    | Nil -> ()
    | Cons(x, xs) -> f x; iter f xs in
  let rec fold_right f xs accu =
    match xs with
    | Nil -> accu
    | Cons(x, xs) -> f x (fold_right f xs accu) in
  let rec map f xs =
    match xs with
    | Nil -> Nil
    | Cons(x, xs) -> Cons(f x, map f xs) in
  let rec suc x = x + 1 in
  let rec add x y = x + y in
  let xs = Cons(11, Cons(12, Cons(13, Nil))) in
  iter print_int xs;
  iter print_int (map suc xs);
  print_int (fold_right add xs 0)


