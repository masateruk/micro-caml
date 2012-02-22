type t = (* uCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Nil of Type.t
  | Bool of bool
  | Int of int
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Cons of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec string_of_exp = function
  | Unit -> "Unit"
  | Nil(t) -> "Nil(" ^ (Type.string_of_typ t) ^ ")"
  | Bool(b) -> "Bool(" ^ (string_of_bool b) ^ ")"
  | Int(n) -> "Int(" ^ (string_of_int n) ^ ")"
  | Not(e) -> "Not(" ^ (string_of_exp e) ^ ")"
  | Neg(e) -> "Neg(" ^ (string_of_exp e) ^ ")"
  | Add(e1, e2) -> "Add(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Sub(e1, e2) -> "Sub(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Mul(e1, e2) -> "Mul(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Div(e1, e2) -> "Div(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Cons(e1, e2) -> "Cons(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Eq(e1, e2) -> "Eq(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | LE(e1, e2) -> "LE(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | If(e1, e2, e3) -> "If(" ^ (string_of_exp e1) ^ " then " ^ (string_of_exp e2) ^ " else " ^ (string_of_exp e3) ^ ")"
  | Let((x, t), e1, e2) -> "Let(" ^ x ^ " : " ^ (Type.string_of_typ t) ^ " = " ^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2) ^ ")"
  | Var(x) -> "Var(" ^ x ^ ")"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> "LetRec(" ^ x ^ "(" ^ (String.concat ", " (List.map (fun (y, t) -> y ^ " : " ^ (Type.string_of_typ t)) yts)) ^ ") : " ^ (Type.string_of_typ t) ^ " = " ^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2) ^ ")"
  | App(e, es) -> "App(" ^ (string_of_exp e) ^ " (" ^ (String.concat ", " (List.map string_of_exp es)) ^ "))"

    
let rec ocaml_of_exp = function
  | Unit -> "()"
  | Nil(t) -> "[] (* : " ^ (Type.string_of_typ t) ^ "*)"
  | Bool(b) -> (string_of_bool b)
  | Int(n) -> (string_of_int n)
  | Not(e) -> "not " ^ (ocaml_of_exp e)
  | Neg(e) -> "- " ^ (ocaml_of_exp e)
  | Add(e1, e2) -> (ocaml_of_exp e1) ^ " + " ^ (ocaml_of_exp e2)
  | Sub(e1, e2) -> (ocaml_of_exp e1) ^ " - " ^ (ocaml_of_exp e2)
  | Mul(e1, e2) -> (ocaml_of_exp e1) ^ " * " ^ (ocaml_of_exp e2)
  | Div(e1, e2) -> (ocaml_of_exp e1) ^ " / " ^ (ocaml_of_exp e2)
  | Cons(e1, e2) -> (ocaml_of_exp e1) ^ " :: " ^ (ocaml_of_exp e2)
  | Eq(e1, e2) -> (ocaml_of_exp e1) ^ " = " ^ (ocaml_of_exp e2)
  | LE(e1, e2) -> (ocaml_of_exp e1) ^ " <= " ^ (ocaml_of_exp e2)
  | If(e1, e2, e3) -> "if " ^ (ocaml_of_exp e1) ^ " then\n" ^ (ocaml_of_exp e2) ^ " else\n" ^ (ocaml_of_exp e3)
  | Let((x, t), e1, e2) -> "let " ^ x ^ " (* : " ^ (Type.string_of_typ t) ^ "*) =\n" ^ (ocaml_of_exp e1) ^ " in\n" ^ (ocaml_of_exp e2)
  | Var(x) -> x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      "let rec " ^ x ^ " " ^ 
	(String.concat ", " (List.map (fun (y, t) -> y ^ " (* : " ^ (Type.string_of_typ t) ^ "*)" ) yts)) ^ " (* : " ^ 
	(Type.string_of_typ (match t with Type.App(Type.Arrow, us) -> L.last us | t -> t)) ^ "*) =\n" ^ 
	(ocaml_of_exp e1) ^ " in\n" ^ (ocaml_of_exp e2)
  | App(e, es) -> "(" ^ (ocaml_of_exp e) ^ " " ^ (String.concat " " (List.map ocaml_of_exp es)) ^ ")"

