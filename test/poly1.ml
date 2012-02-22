let rec id x = x in
let rec f x = id x in 
  print_int (f 0)
