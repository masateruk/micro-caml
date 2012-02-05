type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Var of tyvar
  | App of tycon * t list
  | Poly of tyvar list * t
  | Meta of t option ref (* 型推論であとで代入するために ref 型になっている *)
and tycon =
  | Unit
  | Bool
  | Int
  | Arrow
  | TyFun of tyvar list * t
and tyvar = string
and metavar = string

let counter = ref 0
let newtyvar () = 
  incr counter;
  Printf.sprintf "tyvar_%d" !counter
let newmetavar () = ref None

let rec string_of_typ = function
  | Var(v) -> "Var(" ^ v ^ ")"
  | App(tycon, []) -> "App(" ^ (string_of_tycon tycon) ^ ", [])"
  | App(tycon, ts) -> "App(" ^ (string_of_tycon tycon) ^ ", [" ^ (String.concat "; " (List.map string_of_typ ts)) ^ "])"
  | Poly(xs, t)-> "Poly(" ^ (String.concat ", " xs) ^ ", " ^ (string_of_typ t) ^ ")"
  | Meta{ contents = Some(t) } -> "Meta(Some(" ^ (string_of_typ t) ^ "))"
  | Meta{ contents = None } -> "Meta(none)"

and string_of_tycon = function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Arrow -> "Arrow"
  | TyFun(xs, t) -> "TyFun(" ^ (String.concat ", " xs) ^ " = " ^ (string_of_typ t)

