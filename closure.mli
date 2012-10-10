type closure = { entry : Id.l; actual_fv : Id.t list }
type t = 
  | Unit
  | Nil of Type.t
  | WrapBody of Id.t * Type.t
  | UnwrapBody of Id.t * Type.t
  | Exp of e
  | Cons of Id.t * Id.t
  | If of e * t * t
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | MakeCls of (Id.t * Type.t) * closure * t
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
  | AppDir of Id.l * e list
and pattern =
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}
and def =
  | TypeDef of Id.t * Type.tycon
  | VarDef of (Id.t * Type.t) * t
  | FunDef of fundef
type prog = Prog of def list

val string_of_pattern : pattern -> string
val string_of_e : e -> string
val string_of_exp : t -> string
val string_of_def : def -> string
val fv : t -> S.t
val f : KNormal.def list -> prog
