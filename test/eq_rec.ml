type pos = { x : int; y : int }

let () =
  let rec eq x y = x = y in
  let p1 = { x = 10; y = 12 } in
  let p2 = { x = 10; y = 12 } in
  let xx = (0, 1) in
  if (eq xx xx) then print_int 1 else print_int 0;
  if (eq p1 p2) then print_int 1 else print_int 0


