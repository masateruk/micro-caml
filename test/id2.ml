let rec id x = x in
let rec suc y = y + 1 in
  print_int ((id suc) 2)
