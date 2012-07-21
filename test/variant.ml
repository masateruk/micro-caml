type b = True | False
type nb = MyInt of int | MyBool of bool
type pair = IntPair of int * int | BoolPair of bool * bool

let () = 
  let a = IntPair (11, 12) in
  print_int 0
