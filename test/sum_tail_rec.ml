let rec sum x =
  let rec loop y acc =
    if y <= 0 then 0 else
      loop (y - 1) (acc + y) in
    loop x 0 in
  print_int (sum 1000)

    
