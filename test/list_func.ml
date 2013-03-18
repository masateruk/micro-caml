let rec iter f xs =
  match xs with
  | [] -> ()
  | x :: xs -> f x; iter f xs

let rec fold_right f xs accu =
  match xs with
  | [] -> accu
  | x :: xs -> f x (fold_right f xs accu)

let rec map f xs =
  match xs with
  | [] -> []
  | x :: xs -> f x :: (map f xs)

let () =
  let rec suc x = x + 1 in
  let rec add x y = x + y in
  let xs = [11; 12; 13] in
  iter print_int xs;
  iter print_int (map suc xs);
  print_int (fold_right add xs 0)


