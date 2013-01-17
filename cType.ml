type t =
  | Void 
  | Int
  | Bool
  | Enum of Id.t * Id.t list
  | Fun of t list * t
  | Struct of Id.t * (Id.t * t) list
  | Union of (Id.t * t) list
  | NameTy of Id.t * t option ref
  | Box
  | Pointer of t
  | Pseudo

let rec string_of_t reached t = 
  match t with
  | Void -> "Void" 
  | Bool -> "Bool" 
  | Int -> "Int" 
  | Enum(x, ys) -> "Enum(" ^ x ^ ", [" ^ (String.concat "," ys) ^ "])"
  | Fun(args, ret) -> (string_of_t (t ::reached) ret) ^ " (*)(" ^ (String.concat ", " (List.map (string_of_t (t ::reached)) args)) ^ ")" 
  | Struct(tag, _) when List.mem t reached -> "Struct " ^ tag
  | Struct(tag, xts) -> 
      "Struct " ^ tag ^ " (" ^ 
        (String.concat ", " (List.map (fun (x, t') -> x ^ " : " ^ (string_of_t (t ::reached) t')) xts)) ^ 
        ")" 
  | Union(xts) -> 
      "Union (" ^ 
        (String.concat ", " (List.map (fun (x, t') -> x ^ " : " ^ (string_of_t (t ::reached) t')) xts)) ^ 
        ")" 
  | NameTy(x, { contents = Some(t') }) -> "NameTy(" ^ x ^ ", Some(" ^ (string_of_t (t ::reached) t') ^ "))"
  | NameTy(x, _) -> "NameTy(" ^ x ^ ", None)"
  | Box -> "Box" 
  | Pointer t -> "Pointer(" ^ (string_of_t (t ::reached) t) ^ ")" 
  | Pseudo -> "Pseudo"

let string_of_t = string_of_t []
      
let rec equal t1 t2 = 
  if t1 == t2 then true else
  match t1, t2 with
  | Void, Void | Int, Int | Bool, Bool -> true
  | Enum("", _), Int | Int, Enum("", _) | Enum("", _), Enum("", _) -> true
  | Enum(_, xs), Enum(_, ys) -> xs = ys
  | Fun(args1, ret1), Fun(args2, ret2) -> (try List.for_all2 equal args1 args2 with Invalid_argument _ -> false) && equal ret1 ret2
  | Struct(_, xts), Struct(_, yts) -> (try List.for_all2 (fun (_, t1) (_, t2) -> equal t1 t2) xts yts with Invalid_argument _ -> false)
  | Union(xts), Union(yts) -> (try List.for_all2 (fun (_, t1) (_, t2) -> equal t1 t2) xts yts with Invalid_argument _ -> false)
  | Box, Box -> true
  | Pointer(t1), Pointer(t2) -> equal t1 t2
  | NameTy(_, { contents = Some(x)}), NameTy(_, { contents = Some(y)}) -> equal x y
  | NameTy(_, { contents = Some(x)}), _ -> equal x t2
  | _, NameTy(_, { contents = Some(y)}) -> equal t1 y
  | _ -> false
      
let rec prefix = 
  function
  | Void -> assert false
  | NameTy(x, t') -> x
  | Int -> "n"
  | Bool -> "b"
  | Enum _ -> "e"
  | Fun _ -> "pfn"
  | Struct("", _) -> "st"
  | Struct(x,  _) -> x
  | Union _ -> "u"
  | Box -> "v" 
  | Pointer _ -> assert false
  | Pseudo -> assert false
