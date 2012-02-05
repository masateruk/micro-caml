exception Error of Syntax.t * Type.t * Type.t
val extenv : Type.t M.t ref
val subst : Type.t M.t -> Type.t -> Type.t
val f : Syntax.t -> Syntax.t

