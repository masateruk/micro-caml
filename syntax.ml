type t = (* uCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Nil of Type.t
  | Bool of bool
  | Int of int
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | Tuple of t list
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
  | Constr of Id.t * t list
  | LetRec of fundef * t
  | App of t * t list
  | WrapBody of Id.t * Type.t (* ラップ関数のbody. 外部で定義される扱い。最終的にはc.mlで中身が生成される *)
  | UnwrapBody of Id.t * Type.t 
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
  | Tuple(es) -> "Tuple([" ^ (String.concat "; " (List.map string_of_exp es)) ^ "])"
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
  | Constr(x, es) -> "Constr(" ^ x ^ ", " ^ (String.concat ", " (List.map string_of_exp es)) ^ ")"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> "LetRec(" ^ x ^ "(" ^ (String.concat ", " (List.map (fun (y, t) -> y ^ " : " ^ (Type.string_of t)) yts)) ^ ") : " ^ (Type.string_of t) ^ " = " ^ (string_of_exp e1) ^ " in " ^ (string_of_exp e2) ^ ")"
  | App(e, es) -> "App(" ^ (string_of_exp e) ^ " (" ^ (String.concat ", " (List.map string_of_exp es)) ^ "))"
  | WrapBody(x, t) -> "WrapBody(" ^ x ^ ", " ^ (Type.string_of t) ^ ")"
  | UnwrapBody(x, t) -> "UnwrapBody(" ^ x ^ ", " ^ (Type.string_of t) ^ ")"

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
  | Tuple(es) -> "(" ^ (String.concat ", " (List.map string_of_exp es)) ^ ")"
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
  | Constr(x, []) -> x
  | Constr(x, es) -> "(" ^ x ^ "(" ^ (String.concat ", " (List.map ocaml_of_exp es)) ^ "))"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      "let rec " ^ x ^ " " ^ 
	(String.concat ", " (List.map (fun (y, t) -> y ^ " (* : " ^ (Type.string_of t) ^ "*)" ) yts)) ^ " (* : " ^ 
	(Type.string_of (match t with Type.App(Type.Arrow, us) -> L.last us | t -> t)) ^ "*) =\n" ^ 
	(ocaml_of_exp e1) ^ " in\n" ^ (ocaml_of_exp e2)
  | App(e, es) -> "(" ^ (ocaml_of_exp e) ^ " " ^ (String.concat " " (List.map ocaml_of_exp es)) ^ ")"
  | WrapBody(x, t) -> "(* WrapBody(" ^ x ^ ", " ^ (Type.string_of t) ^ ") *)"
  | UnwrapBody(x, t) -> "(* UnwrapBody(" ^ x ^ ", " ^ (Type.string_of t) ^ ") *)"
      
let fold f defs =
  let _, defs' = 
    List.fold_left
      (fun (env, defs) def -> 
	let venv, tenv = env in
	match def with
	| TypeDef(x, t) -> ((M.add_list (Type.ids t) venv), (M.add_list ((x, t) :: (Type.types t)) tenv)), f (env, defs) def
	| VarDef((x, t), e) -> ((M.add x t venv), tenv), f (env, defs) def
	| RecDef({ name = (x, t); args = yts; body = e }) -> ((M.add x t venv), tenv), f (env, defs) def)
      ((M.empty, M.add_list [("bool", Type.App(Type.Bool, [])); ("int", Type.App(Type.Int, []))] M.empty), []) defs in
  List.rev defs'
    
    
