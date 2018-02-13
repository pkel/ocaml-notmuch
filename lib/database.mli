(** opaque database type *)
type t

(** open database at path. write enables write access. default: r/o *)
val open_ : ?write:bool -> string -> t option

(** commit changes and close database *)
val close : t -> unit

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
