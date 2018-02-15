type t = C.message

val get_tags : t -> string list

val set_tags : t -> string list -> unit
val add_tags : t -> string list -> unit
val  rm_tags : t -> string list -> unit
