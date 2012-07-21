let rec iota n first = 
  if n = 0 then [] else first :: iota (n - 1) (first + 1)
    
let rec init = function
  | [] -> assert false
  | [x] -> []
  | (x::xs) -> x :: (init xs)
      
let last xs = List.hd (List.rev xs)
  
let take n xs = 
  let rec loop n xs acc =
    match xs with
      | [] -> acc
      | (x::xs) -> if n == 0 then acc else loop (n - 1) xs (x::acc) in
    loop n xs []
      
let zip xs ys = 
  let l = min (List.length xs) (List.length ys) in
    List.combine (take l xs) (take l ys)

let rec is_sub eq xs ys =
  match xs, ys with
  | [], _ -> true
  | _, [] -> false
  | (x::xs), (y::ys) -> (eq x y) && (is_sub eq xs ys)

let rec sub eq xs ys =
  match xs, ys with
  | [], _ -> []
  | xs, [] -> xs
  | (x::xs), (y::ys) -> assert (eq x y); sub eq xs ys
