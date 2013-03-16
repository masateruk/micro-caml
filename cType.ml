type t =
  | Void 
  | Int
  | Bool
  | Enum of Id.t * Id.t list
  | Fun of t list * t
  | Struct of Id.t * kind * (Id.t * t) list
  | Union of (Id.t * t) list
  | NameTy of Id.t * t option ref
  | Box
  | Pointer of t
  | Nothing

and kind = Normal | Ref | Closure

let rec string_of_t reached t = 
  match t with
  | Void -> "Void" 
  | Bool -> "Bool" 
  | Int -> "Int" 
  | Enum(x, ys) -> "Enum(" ^ x ^ ", [" ^ (String.concat "," ys) ^ "])"
  | Fun(args, ret) -> "Fun([" ^ (String.concat ", " (List.map (string_of_t (t ::reached)) args)) ^ "] -> " ^ (string_of_t (t ::reached) ret) ^ ")" 
  | Struct(tag, kind, _) when List.mem t reached -> "Struct " ^ tag
  | Struct(tag, kind, xts) -> 
      "Struct " ^ tag ^ ", " ^ (string_of_kind kind) ^ ", (" ^ 
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
  | Nothing -> "Nothing"

and string_of_kind = 
  function
  | Normal -> "Normal"
  | Ref -> "Rec"
  | Closure -> "Closure"

let string_of_t = string_of_t []

let is_ref_pointer =
  let rec is_ref_base =
    function
    | Struct(_, Ref, _) -> true
    | NameTy(_, { contents = Some(t) }) -> is_ref_base t
    | Box | Void | Int | Bool | Enum _ | Fun _ | Union _ | Pointer _ | Nothing | Struct _ | NameTy _ -> false in
  function
  | Box -> true
  | Pointer(t) -> is_ref_base t
  | Void | Int | Bool | Enum _ | Fun _ | Union _ | Nothing | Struct _ | NameTy _ -> false

let rec identical t1 t2 = 
  t1 == t2 ||
  match t1, t2 with
  | Void, Void | Int, Int | Bool, Bool | Box, Box | Nothing, Nothing -> true
  | Fun(args1, ret1), Fun(args2, ret2) -> (L.for_all2 identical args1 args2) && identical ret1 ret2
  | Enum("", xs), Enum("", ys) -> xs = ys
  | Enum(x, _), Enum(y, _) -> x = y
  | Struct("", k1, xts), Struct("", k2, yts) -> k1 = k2 && L.for_all2 (fun (_, t1) (_, t2) -> identical t1 t2) xts yts
  | Struct(x, _, _), Struct(y, _, _) -> x = y
  | Union(xts), Union(yts) -> L.for_all2 (fun (_, t1) (_, t2) -> identical t1 t2) xts yts
  | Pointer(t1), Pointer(t2) -> identical t1 t2
  | NameTy(x, _), NameTy(y, _) when x = y -> true
  | NameTy(_, { contents = Some(t)}), _ -> identical t t2
  | _, NameTy(_, { contents = Some(t)}) -> identical t1 t
  | _ -> false

let rec equal t1 t2 = 
  identical t1 t2 ||
  match t1, t2 with
  | Void, Void | Int, Int | Bool, Bool | Box, Box | Nothing, Nothing 
  | Enum("", _), Int | Int, Enum("", _) | Enum("", _), Enum("", _) 
    -> true
  | Fun(args1, ret1), Fun(args2, ret2) 
    -> (L.for_all2 equal args1 args2) && equal ret1 ret2
  | Enum(_, xs), Enum(_, ys) 
    -> xs = ys
  | Struct(_, k1, xts), Struct(_, k2, yts)
    -> k1 = k2 && L.for_all2 (fun (_, t1) (_, t2) -> equal t1 t2) xts yts
  | Union(xts), Union(yts) 
    -> L.for_all2 (fun (_, t1) (_, t2) -> equal t1 t2) xts yts
  | Pointer(t1), Pointer(t2) 
    -> equal t1 t2
  | NameTy(_, { contents = Some(t)}), _ -> equal t t2
  | _, NameTy(_, { contents = Some(t)}) -> equal t1 t
  | _ -> false

let rec prefix = 
  function
  | Void -> assert false
  | NameTy(x, t') -> x
  | Int -> "n"
  | Bool -> "b"
  | Enum _ -> "e"
  | Fun _ -> "pfn"
  | Struct("", _, _) -> "st"
  | Struct(x, _, _) -> x
  | Union _ -> "u"
  | Box -> "p" 
  | Pointer _ -> "p"
  | Nothing -> assert false

let reflist = ref None
let list_body = Struct("list", Ref, [("type", Int); ("u", Union([("Cons", Struct("Cons", Normal, [("head", Box); ("tail", Pointer(NameTy("list", reflist)))])); ("Nil", Nothing)]))])
let list = 
  let list = NameTy("list", ref (Some(list_body))) in
  reflist := Some(list);
  list
