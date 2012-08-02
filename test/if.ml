let () = 
  let rec bool_of_int x = 
    if x = 0 then false else true in
  let a = 
    if (true && true) then print_int 1 else print_int 0 in
  let b = 
    if (true && let c = true in c) then print_int 1 else print_int 0 in
  let n = 
    if bool_of_int 1 then 10 else 20 in
  let d = 
    if (n = 10) then 11 else if (n = 12) then let e = 13 in e + 1 else 14 in
  let e = 
    if (n = 10) then 11 else if (n = 12) then 13 else 14 in
  print_int d
    
    
