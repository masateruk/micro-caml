type 'a ref = { contents : 'a }

let () = 
(*
  let rec id x = x in
  let f = { contents = id } in
*)
  let n = { contents = 10 } in
  let b = { contents = true } in
  print_int n.contents;
  if (b.contents) then print_int 1 else print_int 0
