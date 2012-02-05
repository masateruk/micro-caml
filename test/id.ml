let rec id x = x in
let n = if (id true) then (id 1) else (id 0) in
  print_int n
