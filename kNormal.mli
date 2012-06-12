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
  | Record of (Id.t * e) list
  | Field of e * Id.t
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
and def =
  | TypeDef of (Id.t * Type.t)
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef
val f : Syntax.def list -> def list
val ocaml_of_e : e -> Id.t
val ocaml_of_t : t -> Id.t
val fold :
  ('a M.t * 'b M.t -> def -> 'c) ->
  (Id.t -> Type.t -> 'a M.t -> 'a M.t) ->
  (Id.t -> Type.t -> 'b M.t -> 'b M.t) -> def list -> 'c list

