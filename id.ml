type t = string (* 変数の名前 (caml2html: id_t) *)
type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s_%d_" s !counter

let rec string_of_typ = function
  | Type.Unit -> "unit"
  | Type.Bool -> "bool"
  | Type.Int -> "int"
  | Type.Fun _ -> "function"
  | Type.Var _ -> "var"
  | Type.Closure _ -> "closure"
  | Type.Record _ -> "record"
  | Type.Name(s, _) -> s

let rec ml_of_typ = function
  | Type.Unit -> "()"
  | Type.Bool -> "bool"
  | Type.Int -> "int"
  | Type.Fun _ -> "fun"
  | Type.Var _ -> assert false
  | Type.Closure _ -> assert false
  | Type.Record _ -> assert false
  | Type.Name _ -> assert false

let gentmp typ =
  incr counter;
  Printf.sprintf "tmp_%s%d" (Type.id_of_typ typ) !counter

let gentype typ = 
  incr counter;
  Printf.sprintf "%s_%d_t" (Type.id_of_typ typ) !counter
