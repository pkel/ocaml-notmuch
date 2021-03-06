(** opaque database type *)
type t = C.database

(** open database at path. write enables write access. default: r/o *)
val open_ : ?write:bool -> string -> t option

(** commit changes and close database *)
val close : t -> unit

(** get base folder *)
val get_path : t -> string

(** opaque revision handling *)
type revision

val get_revision : t -> revision

type rev_comp_result =
  | Newer
  | Older
  | Same
  | Incompatible

val rev_compare : revision -> revision -> rev_comp_result

(** revision serialization *)
val string_of_revision : revision -> string
val revision_of_string : string -> revision option

(** get uuid and revision number separately **)
val uuid_of_revision : revision -> string
val nr_of_revision   : revision -> string

(** get all used tags *)
val get_all_tags : t -> string list
