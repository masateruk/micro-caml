type t =
  | Unit
  | Nil of Type.t
  | Exp of e
  | Cons of Id.t * Id.t
  | If of e * t * t 
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t
and e =
  | Bool of bool
  | Int of int
  | Not of e
  | Neg of e
  | Add of e * e
  | Sub of e * e
  | Mul of e * e
  | Div of e * e
  | Eq of e * e
  | LE of e * e
  | Var of Id.t
  | App of e * e list
  | ExtFunApp of Id.t * e list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
val f : Syntax.t -> t
val ocaml_of_expr : t -> Id.t
