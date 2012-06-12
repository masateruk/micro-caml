let n = 1

let () = 
  let rec plus x =
    let rec add y = n + x + y in
      add in
    print_int ((plus 1) 2)
      
