type t = 
    | Var of tyvar 
    | App of tycon * t list 
    | Poly of tyvar list * t
    | Meta of t option ref
and tycon = 
    | Unit 
    | Bool 
    | Int 
    | Arrow 
    | TyFun of tyvar list * t
and tyvar = string
and metavar = string
val counter : int ref
val newtyvar : unit -> string
val newmetavar : unit -> t option ref
val string_of_typ : t -> string
val string_of_tycon : tycon -> string
