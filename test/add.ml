let rec plus x =
  let rec add y = x + y in
    add in
  print_int ((plus 1) 2)
