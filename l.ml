let rec iota n first = 
  if n = 0 then [] else first :: iota (n - 1) (first + 1)
    
let rec init = function
  | [] -> assert false
  | [x] -> []
  | (x::xs) -> x :: (init xs)
      
let last xs = List.hd (List.rev xs)

