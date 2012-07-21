type t = { n : int; b : bool }

let r = { n = 1; b = true }

let () = 
  let a = { n = 0; b = false } in
  let b = 
    let rec id x = x in
    id a in
  print_int b.n;
  print_int r.n

