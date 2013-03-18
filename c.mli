type t =
  | Dec of (Id.t * CType.t) * expr option
  | Assign of expr * expr
  | Exp of expr
  | If of expr * t * t
  | Return of expr
  | Seq of t * t
  | Block of dec list * t
and dec =
  | VarDec of (Id.t * CType.t) * expr option
and expr =
  | Nop
  | Bool of bool
  | Int of int
  | Struct of Id.t * (Id.t * expr) list
  | FieldDot of expr * Id.t
  | FieldArrow of expr * Id.t
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | LE of expr * expr
  | Cons of Id.t * Id.t
  | Var of Id.t
  | Cond of expr * expr * expr
  | CallDir of expr * expr list
  | Let of (Id.t * CType.t) * expr * expr
  | MakeClosure of Id.l * Id.t * (Id.t * CType.t) list
  | Sizeof of CType.t
  | Ref of expr
  | Deref of expr
  | Cast of CType.t * expr
  | Comma
type fundef = {
  name : Id.l;
  args : (Id.t * CType.t) list;
  body : t;
  ret : CType.t;
}
type def =
  | VarDef of (Id.t * CType.t) * t
  | FunDef of fundef * bool ref (* used flag *)
  | TypeDef of (Id.t * CType.t) * bool ref
  | EnumDef of Id.t list * bool ref
type prog = Prog of def list
  
val string_of_expr : expr -> Id.t
val enable_gc : bool ref
val f : Closure.prog -> prog

