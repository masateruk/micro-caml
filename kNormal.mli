type t = (* K正規化後の式 (caml2html: knormal_t) *)
    term * Type.t
and term =
  | Unit
  | Exp of e
  | If of e * t * t 
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t
  | WrapBody of Id.t * Type.t
  | UnwrapBody of Id.t * Type.t
and e = 
    expr * Type.t
and expr =
  | Bool of bool
  | Int of int
  | Record of (Id.t * e) list
  | Field of e * Id.t
  | Tuple of e list
  | Not of e
  | And of e * e
  | Or of e * e
  | Neg of e
  | Add of e * e
  | Sub of e * e
  | Mul of e * e
  | Div of e * e
  | Eq of e * e
  | LE of e * e
  | Var of Id.t
  | Constr of Id.t * e list
  | App of e * e list
  | ExtFunApp of Id.t * e list
and pattern =
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and def =
  | TypeDef of (Id.t * Type.tycon)
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef
val f : Syntax.def list -> def list
val string_of_typed_expr : e -> Id.t
val string_of_expr : expr -> Id.t
val string_of_typed_term : t -> Id.t
val string_of_term : term -> Id.t
val fold : ('a * 'b list -> 'c -> 'a * 'b list) -> 'a -> 'c list -> 'b list
val map : (Env.t -> def -> 'a) -> def list -> 'a list
