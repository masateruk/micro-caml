let rec id x = x

let () = 
  let f = 
    let rec suc x = (id x) + 1 in
    suc in
  print_int (f 0)
