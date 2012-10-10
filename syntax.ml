type t = (* uCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Nil of Type.t
  | Bool of bool
  | Int of int
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | Tuple of t list
  | Not of t
  | And of t * t
  | Or of t * t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Cons of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Match of t * (pattern * t) list
  | LetVar of (Id.t * Type.t) * t * t
  | Var of Id.t
  | Constr of Id.t * t list
  | LetRec of fundef * t
  | App of t * t list
  | WrapBody of Id.t * Type.t (* ラップ関数のbody. 外部で定義される扱い。最終的にはc.mlで中身が生成される *)
  | UnwrapBody of Id.t * Type.t 
and pattern =
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and def =
  | TypeDef of Id.t * Type.tycon
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef

let rec string_of_pattern =
  function
  | PtBool(b) -> "PtBool(" ^ (string_of_bool b) ^ ")"
  | PtInt(n) -> "PtInt(" ^ (string_of_int n) ^ ")"
  | PtVar(x, t) -> "PtVar(" ^ x ^ "," ^ (Type.string_of t) ^ ")"
  | PtTuple(ps) -> "PtTuple([" ^ (String.concat "; " (List.map string_of_pattern ps)) ^ "])"
  | PtField(xps) -> "PtField([" ^ (String.concat "; " (List.map (fun (x, p) -> x ^ ", " ^ (string_of_pattern p)) xps)) ^ "])"
  | PtConstr(x, ps) -> "PtConstr(" ^ x ^ ", [" ^ (String.concat "; " (List.map string_of_pattern ps)) ^ "])"

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
  | And(e1, e2) -> "And(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Or(e1, e2) -> "Or(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Neg(e) -> "Neg(" ^ (string_of_exp e) ^ ")"
  | Add(e1, e2) -> "Add(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Sub(e1, e2) -> "Sub(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Mul(e1, e2) -> "Mul(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Div(e1, e2) -> "Div(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Cons(e1, e2) -> "Cons(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | Eq(e1, e2) -> "Eq(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | LE(e1, e2) -> "LE(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")"
  | If(e1, e2, e3) -> "If(" ^ (string_of_exp e1) ^ " then " ^ (string_of_exp e2) ^ " else " ^ (string_of_exp e3) ^ ")"
  | Match(e, pes) -> "Match(" ^ (string_of_exp e) ^ ", [" ^ (String.concat "; " (List.map (fun (p, e) -> (string_of_pattern p) ^ " -> " ^ (string_of_exp e)) pes)) ^ "])"
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
  | TypeDef(x, t) -> "TypeDef(" ^ x ^ ", " ^ (Type.string_of_tycon t) ^ ")"
  | VarDef((x, t), e) -> "VarDef((" ^ x ^ ", " ^ (Type.string_of t) ^ "), " ^ (string_of_exp e)
  | RecDef(fundef) -> "RecDef(" ^ (string_of_fundef fundef) ^ ")"
      
let fold f defs =
  let _, defs' = 
    List.fold_left
      (fun ({ Env.venv = var_types; types = types; tycons = tycons } as env, defs) def -> 
        match def with
        | TypeDef(x, t) -> 
            { env with 
              Env.types  = M.add_list (Type.types t) types; 
              Env.tycons = M.add_list ((x, t) :: (Type.tycons t)) tycons }, 
          f (env, defs) def
        | VarDef((x, t), e) -> 
            Env.add_var_type env x t, f (env, defs) def
        | RecDef({ name = (x, t); args = yts; body = e }) -> 
            Env.add_var_type env x t, f (env, defs) def)
      (Env.empty, []) defs in
  List.rev defs'
