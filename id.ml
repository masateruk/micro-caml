type t = string (* 変数の名前 (caml2html: id_t) *)
type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "_%s%d" s !counter
  
let rec id_of_typ = function
  | Type.Var _ -> "v" 
  | Type.App(tycon, _) -> id_of_tycon tycon
  | Type.Poly(_, t) -> id_of_typ t
  | _ -> assert false

and id_of_tycon = function
    | Type.Unit -> "u"
    | Type.Bool -> "b"
    | Type.Int -> "n"
    | Type.Arrow -> "pfn"
    | Type.TyFun(_, t) -> id_of_typ t

let gentmp typ = genid (id_of_typ typ)

let rec ocaml_of_typ = function
  | Type.Var _ -> "'a"
  | Type.App(Type.Unit, []) -> "()"
  | Type.App(Type.Bool, []) -> "bool"
  | Type.App(Type.Int, []) -> "int"
  | Type.App(Type.Arrow, xs) -> String.concat " -> " (List.map ocaml_of_typ xs)
  | Type.Poly([], t) -> ocaml_of_typ t      
  | _ -> assert false
    
