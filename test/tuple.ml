type pos = { x : int; y : int }
let () = 
  let rec id x = x in
  let c = 1, false, { x = 10; y = 25 } in
  let d = id c in
  print_int 0
