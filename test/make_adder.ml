let a = 
  let rec make_adder x =
    let rec adder y = x + y in
      if true then adder else adder in
    (make_adder 3) 7 in
  print_int a
