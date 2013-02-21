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
  let rec cons z zs = Cons(z, zs) in
  let xs = Cons(12, Cons(13, Nil)) in
  let ys = fold_right cons xs Nil in
  iter print_int xs;
  iter print_int ys





