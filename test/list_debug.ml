let () =
  let rec iter f xs =
    match xs with
    | [] -> ()
    | x :: xs -> f x; iter f xs in
  let rec fold_right f xs accu =
    match xs with
    | [] -> accu
    | x :: xs -> f x (fold_right f xs accu) in
  let rec cons z zs = z :: zs in
  let xs = [12; 13] in
  let ys = fold_right cons xs [] in
  iter print_int xs;
  iter print_int ys





