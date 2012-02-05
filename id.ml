type t = string (* 変数の名前 (caml2html: id_t) *)
type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s%d" s !counter
  
let rec id_of_typ = function
  | Type.Var _ -> "v" 
  | Type.App(tycon, _) -> id_of_tycon tycon
  | Type.Poly(_, t) -> id_of_typ t
  | _ -> assert false

and id_of_tycon = function
    | Type.Unit -> "u"
    | Type.Bool -> "b"
    | Type.Int -> "n"
    | Type.Arrow -> "a"
    | Type.TyFun(_, t) -> id_of_typ t

let gentmp typ =
  incr counter;
  Printf.sprintf "tmp_%s%d" (id_of_typ typ) !counter
