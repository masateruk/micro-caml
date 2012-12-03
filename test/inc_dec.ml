let () =
  let rec inc x = x + 1 in
  let rec dec y = y - 1 in
  let rec id z = z in
  let rec print0_id x = print_int 999; x in
  let f = if true then id else dec in
  print_int (f 10);
  print_int ((if true then id else id) 11);
  print_int ((if true then print0_id else id) 10);
  let n = 10 in
  let g = 
    match n with
    | 10 -> dec
    | n -> id in
  print_int (g 11)

