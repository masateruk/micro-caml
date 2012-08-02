type pos = { x : int; y : int }
let () = 
  let rec id x = x in
  let a = 1 in
  let b = true in
  let c = 1, false, { x = 10; y = 25 } in
  let e = a, b, c in
  let d = id c in
  print_int 0
