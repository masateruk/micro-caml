type 'a ref = { contents : 'a }

let () = 
  let rec id x = x in
  let rec add x y = x + y in
  let f = { contents = id } in
  let g = { contents = add } in
  let n = { contents = 10 } in
  let b1 = { contents = true } in
  let b2 = { contents = true } in
  print_int (f.contents 10);
  print_int (f.contents 11);
  print_int (g.contents 12 13);
  if (b1 = b2) then print_int (n.contents) else print_int 0
