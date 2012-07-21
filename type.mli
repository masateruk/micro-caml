type t = 
  | Var of tyvar 
  | Field of t * t
  | App of tycon * t list 
  | Poly of tyvar list * t
  | Meta of t option ref
  | NameTy of Id.t * t option ref
and tycon = 
  | Unit 
  | Bool 
  | Int 
  | Arrow 
  | Tuple
  | Record of Id.t * Id.t list
  | Variant of Id.t * (Id.t * t list) list
  | TyFun of tyvar list * t
and tyvar = Id.t
and metavar = Id.t
val counter : int ref
val newtyvar : unit -> Id.t
val newmetavar : unit -> t option ref
val string_of : t -> Id.t
val string_of_tycon : tycon -> Id.t
val prefix : t -> Id.t
val ocaml_of : t -> Id.t
val equal : t -> t -> bool
val apply : t -> t list -> t
val ids : t -> (Id.t * t) list
val types : t -> (Id.t * t) list
val name : t -> Id.t
