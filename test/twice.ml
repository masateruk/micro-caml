let () = 
  let rec twice f x = f (f x) in
  let rec suc x = x + 1 in
  let y = twice suc 0 in
    print_int y

