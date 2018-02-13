type t

val get_tags : t -> Tags.t
val get_tag_list : t -> Tag.t list

val set_tags : t -> Tag.t list -> unit
val add_tags : t -> Tag.t list -> unit
val  rm_tags : t -> Tag.t list -> unit
