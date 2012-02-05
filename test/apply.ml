let rec apply f x = f x in
let rec suc y = y + 1 in
let n = apply suc 0 in
  print_int n

