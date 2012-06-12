let () = 
  let rec bool_of_int x = 
    if x = 0 then false else true in
  let n = if bool_of_int 1 then 10 else 20 in
    print_int n
