type t = { n : int; b : bool }

let r = { n = 1; b = true }

let () = 
  let a = { n = 0; b = false } in
  print_int a.n;
  print_int r.n

