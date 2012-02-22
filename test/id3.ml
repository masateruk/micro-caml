let rec id x = x in
let rec add x y = x + y in
  print_int ((id add) 2 3)
