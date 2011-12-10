type t =
    Unit
  | Bool
  | Int
  | Fun of t list * t * t list
  | Var of t option ref
  | Closure of t * t list
  | Record of (string * t) list
  | Name of string * t
val gentyp : unit -> t
val id_of_typ : t -> string
val string_of_typ : t -> string
val eq : t -> t -> bool
val apply : t -> t list -> t
