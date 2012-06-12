let () = 
  let rec apply_plus_100 f x = (f x) + 100 in
  let rec id x = x in
    print_int (apply_plus_100 id 1)
      
