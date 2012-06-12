type t = (* uCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Nil of Type.t
  | Bool of bool
  | Int of int
  | Record of (Id.t * t) list
  | Field of t * Id.t
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
  | LetVar of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and def =
  | TypeDef of Id.t * Type.t
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef

let rec string_of_exp = 
  function
  | Unit -> "Unit"
  | Nil(t) -> "Nil(" ^ (Type.string_of t) ^ ")"
  | Bool(b) -> "Bool(" ^ (string_of_bool b) ^ ")"
  | Int(n) -> "Int(" ^ (string_of_int n) ^ ")"
  | Record(xs) -> "Record(" ^ (String.concat "; " (List.map (fun (x, e) -> x ^ " = " ^ (string_of_exp e)) xs)) ^ ")"
  | Field(e, x) -> "Field(" ^ (string_of_exp e) ^ ", " ^ x ^ ")"
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
  | LetVar((x, t), e1, e2) -> "LetVar(" ^ x ^ " : " ^ (Type.string_of t) ^ " = " ^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2) ^ ")"
  | Var(x) -> "Var(" ^ x ^ ")"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> "LetRec(" ^ x ^ "(" ^ (String.concat ", " (List.map (fun (y, t) -> y ^ " : " ^ (Type.string_of t)) yts)) ^ ") : " ^ (Type.string_of t) ^ " = " ^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2) ^ ")"
  | App(e, es) -> "App(" ^ (string_of_exp e) ^ " (" ^ (String.concat ", " (List.map string_of_exp es)) ^ "))"

let string_of_fundef { name = (x, t); args = yts; body = e } =
  x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) yts)) ^ " : " ^ (Type.string_of t) ^ " = " ^ (string_of_exp e) 

let rec string_of_def = function
  | TypeDef(x, t) -> "TypeDef(" ^ x ^ ", " ^ (Type.string_of t) ^ ")"
  | VarDef((x, t), e) -> "VarDef((" ^ x ^ ", " ^ (Type.string_of t) ^ "), " ^ (string_of_exp e)
  | RecDef(fundef) -> "RecDef(" ^ (string_of_fundef fundef) ^ ")"
      
let rec ocaml_of_exp = 
  function
  | Unit -> "()"
  | Nil(t) -> "[] (* : " ^ (Type.string_of t) ^ "*)"
  | Bool(b) -> (string_of_bool b)
  | Int(n) -> (string_of_int n)
  | Record(xs) -> "{" ^ (String.concat "; " (List.map (fun (x, e) -> x ^ " = " ^ (ocaml_of_exp e)) xs)) ^ "}"
  | Field(e, x) -> (ocaml_of_exp e) ^ ", " ^ x
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
  | LetVar((x, t), e1, e2) -> "let " ^ x ^ " (* : " ^ (Type.string_of t) ^ "*) =\n" ^ (ocaml_of_exp e1) ^ " in\n" ^ (ocaml_of_exp e2)
  | Var(x) -> x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      "let rec " ^ x ^ " " ^ 
	(String.concat ", " (List.map (fun (y, t) -> y ^ " (* : " ^ (Type.string_of t) ^ "*)" ) yts)) ^ " (* : " ^ 
	(Type.string_of (match t with Type.App(Type.Arrow, us) -> L.last us | t -> t)) ^ "*) =\n" ^ 
	(ocaml_of_exp e1) ^ " in\n" ^ (ocaml_of_exp e2)
  | App(e, es) -> "(" ^ (ocaml_of_exp e) ^ " " ^ (String.concat " " (List.map ocaml_of_exp es)) ^ ")"

let rec id_and_types x t =
  let () = D.printf "Syntax.id_and_types %s\n" (Type.string_of t) in
  match t with
  | Type.App(Type.Record(x, fs), ys) as t -> (x, t) :: (List.combine fs (List.map (fun y -> Type.Field(t, y)) ys))
  | t -> [(x, t)]

let fold f defs =
  let _, defs' = 
    List.fold_left
      (fun (env, defs) def -> 
	let venv, tenv = env in
	  match def with
	  | TypeDef(x, t) -> (venv, (M.add_list (id_and_types x t) tenv)), (f env def) :: defs
	  | VarDef((x, t), e) -> ((M.add x t venv), tenv), (f env def) :: defs
	  | RecDef({ name = (x, t); args = yts; body = e }) -> ((M.add x t venv), tenv), (f env def) :: defs)
      ((M.empty, M.add_list [("bool", Type.App(Type.Bool, [])); ("int", Type.App(Type.Int, []))] M.empty), []) defs in
    List.rev defs'
