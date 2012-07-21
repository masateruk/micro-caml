type t =
  | Dec of (Id.t * CType.t) * exp option
  | Assign of exp * exp
  | Exp of exp
  | If of exp * t * t
  | Return of exp
  | Seq of t * t
  | Block of dec list * t
and dec =
  | VarDec of (Id.t * CType.t) * exp option
and exp =
  | Nop
  | Nil of CType.t
  | Bool of bool
  | Int of int
  | Struct of Id.t * (Id.t * exp) list
  | Field of exp * Id.t
  | Not of exp
  | Neg of exp
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | Eq of exp * exp
  | LE of exp * exp
  | Cons of Id.t * Id.t
  | Var of Id.t
  | Cond of exp * exp * exp
  | CallDir of exp * exp list
  | Let of (Id.t * CType.t) * exp * exp
  | MakeClosure of Id.l * Id.t * (Id.t * CType.t) list
  | Sizeof of CType.t
  | Ref of exp
  | Deref of exp
  | Cast of CType.t * exp
  | Comma
type fundef = {
  name : Id.l;
  args : (Id.t * CType.t) list;
  body : t;
  ret : CType.t;
}
type def = 
  | VarDef of (Id.t * CType.t) * t
  | FunDef of fundef * bool ref 
  | TypeDef of (Id.t * CType.t) * bool ref
  | EnumDef of Id.t list * bool ref
type prog = Prog of def list
  
val enable_gc : bool ref
val f : Closure.prog -> prog

