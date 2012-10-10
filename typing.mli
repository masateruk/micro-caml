exception Error of Syntax.t * Type.t * Type.t
val subst : Env.t -> Type.t M.t -> Type.t -> Type.t
val f : Syntax.def list -> Syntax.def list

