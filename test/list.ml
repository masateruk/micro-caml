let () = 
  let e = [] in
  let xs = 11 :: e in
  let ys = 12 :: xs in
  let bs = true :: e in
  (match xs with
  | [] -> print_int 0
  | x :: _ -> print_int x);
  (match ys with
  | [] -> print_int 0
  | y :: (z :: _) -> print_int y; print_int z
  | _ -> print_int 256);
  (match bs with
  | [] -> print_int 0
  | b :: _ -> if b then (print_int 1) else (print_int 0));
  ()
