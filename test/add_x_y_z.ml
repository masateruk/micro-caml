let a = 
  let rec do_add_with x =
    let rec add_y y = 
      let rec add_z z = x + y + z in
      let rec diff_z z = x + y - z in
	if true then add_z else diff_z in
      (add_y 1) in
    ((do_add_with 2) 3) in
  print_int a
