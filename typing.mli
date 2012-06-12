exception Error of Syntax.t * Type.t * Type.t
val extenv : Type.t M.t ref
val equal : Type.t M.t -> Type.t -> Type.t -> bool
val subst : Type.t M.t * Type.t M.t -> Type.t -> Type.t
val f : Syntax.def list -> Syntax.def list

