let () = 
  let rec id x = x in
  let rec f x = 
    let y = (id x) + 1 in 
      y in
    print_int (f 0)
      
      
