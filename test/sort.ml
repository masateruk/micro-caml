let rec iter f xs =
  match xs with
  | [] -> ()
  | x :: xs -> f x; iter f xs

let rec append xs ys = 
  match xs with
  | [] -> ys
  | x :: xs -> x :: (append xs ys)

let rec filter p xs =
  match xs with
  | [] -> []
  | x :: xs -> if (p x) then x :: (filter p xs) else (filter p xs)

let rec sort xs =
  match xs with
  | [] -> []
  | x :: xs -> 
      let rec le y = (y <= x) in
      let rec gt y = (y > x) in
      let left = sort (filter le xs) in
      let right = sort (filter gt xs) in
      append left (x :: right)

let rec is_sorted xs =
  match xs with
  | [] -> true
  | [x] -> true
  | x :: y :: zs -> (x <= y) && (is_sorted (y::zs))

let () = 
  let xs = [5; 2; 3] in
  let ys = sort xs in
  assert (is_sorted ys);
  iter print_int ys
