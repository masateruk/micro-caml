let () = 
  let rec id x = x in
  let rec fst x y = 
    let z = id x in
      z in
  let n = 1 in
  let m = fst n n in
    print_int m
      
