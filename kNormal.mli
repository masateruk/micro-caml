type t =
  | Unit
  | Nil of Type.t
  | Exp of e
  | Cons of Id.t * Id.t
  | If of e * t * t 
  | MATCH of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t
  | WrapBody of Id.t * Type.t
  | UnwrapBody of Id.t * Type.t
and e =
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
  | PtVar of Id.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and def =
  | TypeDef of (Id.t * Type.t)
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef
val f : Syntax.def list -> def list
val ocaml_of_e : e -> Id.t
val ocaml_of_t : t -> Id.t
val fold : ('a * 'b list -> 'c -> 'a * 'b list) -> 'a -> 'c list -> 'b list
val map : (Type.t M.t * Type.t M.t -> def -> 'a) -> def list -> 'a list
