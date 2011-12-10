type t = 
    | Dec of (Id.t * Type.t) * exp option
    | Assign of exp * exp
    | Exp of exp 
    | IfEq of Id.t * Id.t * t * t
    | IfLE of Id.t * Id.t * t * t
    | Return of exp
    | Seq of t * t
    | Block of dec list * t
and dec =
    | VarDec of (Id.t * Type.t) * exp option
and exp =
    Nop
    | Bool of bool
    | Int of int
    | Not of Id.t
    | Neg of Id.t
    | Add of Id.t * Id.t
    | Sub of Id.t * Id.t
    | Mul of Id.t * Id.t
    | Div of Id.t * Id.t
    | Var of Id.t
    | Fun of Id.t
    | CondEq of Id.t * Id.t * exp * exp
    | CondLE of Id.t * Id.t * exp * exp
    | CallCls of (Id.t * Type.t) * Id.t list
    | CallDir of exp * exp list
    | MakeClosure of Id.l * Id.l * (Id.t * Type.t) list
    | Field of Id.t * Id.t
type fundef = {
  name : Id.l;
  args : (Id.t * Type.t) list;
  body : t;
  ret : Type.t;
}
type def = FunDef of fundef | TypeDef of (Id.t * Type.t)
type prog = Prog of def list * t

val f : Closure.prog -> string
