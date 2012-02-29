type closure = { entry : Id.l; actual_fv : Id.t list }
type t = 
  | Unit
  | Nil of Type.t
  | Exp of e
  | Cons of Id.t * Id.t
  | If of e * t * t
  | Let of (Id.t * Type.t) * t * t
  | MakeCls of (Id.t * Type.t) * closure * t
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
  | AppDir of Id.l * e list
type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}
type prog = Prog of fundef list * t

val string_of_e : e -> string
val string_of_exp : t -> string
val fv : t -> S.t
val f : KNormal.t -> prog
