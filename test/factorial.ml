let rec factorial x =
  if x = 0 then 1 else
    x * factorial (x - 1) in
  print_int (factorial 5)
