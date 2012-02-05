type ty =
  | Void 
  | Int
  | Bool
  | Fun of ty list * ty
  | Struct of (Id.t * ty) list
  | NameTy of Id.t * ty
  | Box
  | Pointer of ty
type t =
  | Dec of (Id.t * ty) * exp option
  | Assign of exp * exp
  | Exp of exp
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Return of exp
  | Seq of t * t
  | Block of dec list * t
and dec =
  | VarDec of (Id.t * ty) * exp option
and exp =
  | Nop
  | Nil of ty
  | BoolExp of bool
  | IntExp of int
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | Cons of Id.t * Id.t
  | Var of Id.t
  | CondEq of Id.t * Id.t * exp * exp
  | CondLE of Id.t * Id.t * exp * exp
  | CallCls of (Id.t * ty) * Id.t list
  | CallDir of exp * exp list
  | MakeClosure of Id.l * Id.t * (Id.t * ty) list
  | Field of Id.t * Id.t
  | Sizeof of ty
  | Ref of exp
  | Deref of exp
  | Cast of ty * exp
type fundef = {
  name : Id.l;
  args : (Id.t * ty) list;
  body : t;
  ret : ty;
}
type def = FunDef of fundef | TypeDef of (Id.t * ty)
type prog = Prog of def list * t
  
val f : Closure.prog -> string
