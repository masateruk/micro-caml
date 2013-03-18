let rec id x = x

let f = 
  let rec suc x = (id x) + 1 in
  suc

let () = 
  print_int (f 0)
