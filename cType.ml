type t =
  | Void 
  | Int
  | Bool
  | Fun of t list * t
  | Struct of Id.t * (Id.t * t) list
  | NameTy of Id.t * t option ref
  | Box
  | Pointer of t

let rec equal t1 t2 = 
  match t1, t2 with
  | Void, Void | Int, Int | Bool, Bool -> true
  | Fun(args1, ret1), Fun(args2, ret2) -> (try List.for_all2 equal args1 args2 with Invalid_argument _ -> false) && equal ret1 ret2
  | Struct(_, xts), Struct(_, yts) -> (try List.for_all2 (fun (_, t1) (_, t2) -> equal t1 t2) xts yts with Invalid_argument _ -> false)
  | Box, Box -> true
  | Pointer(t1), Pointer(t2) -> equal t1 t2
  | NameTy(_, { contents = Some(x)}), NameTy(_, { contents = Some(y)}) -> equal x y
  | NameTy(_, { contents = Some(x)}), _ -> equal x t2
  | _, NameTy(_, { contents = Some(y)}) -> equal t1 y
  | _ -> false
      
let rec prefix = 
  function
  | Void -> assert false
  | Int -> "n"
  | Bool -> "b"
  | Fun _ -> "pfn"
  | Struct _ -> "st"
  | NameTy(x, t') -> x
  | Box -> "v" 
  | Pointer _ -> assert false
