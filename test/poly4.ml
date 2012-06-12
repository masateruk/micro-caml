let () = 
  let rec id x = x in
  let rec f y = 
    let z = id y in 
      z in
    print_int (f 0)
      
      
