let rec id x = x in
let n = if 0 < (id 1) then (id 10) else (id 20) in
  print_int n
