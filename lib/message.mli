type t = C.message

val get_id : t -> string

(** Tags *)
val get_tags : t -> string list
val set_tags : t -> string list -> unit
val add_tags : t -> string list -> unit
val remove_tags : t -> string list -> unit

(** Filenames *)
val get_filenames : t -> string list
