open Ctypes

type status = int
val status_t : status typ

type tags = unit ptr
val tags_t : tags typ

type database = unit ptr
val database_t : database typ

type database_mode = int
val database_mode_t : database_mode typ

(** Database basics *)
val database_open : string -> database_mode -> database ptr -> status
val database_close : database -> status
val database_get_revision : database -> string ptr -> Unsigned.ULong.t
val database_get_all_tags : database -> tags

(** Tags iterator *)
val tags_valid : tags -> bool
val tags_get : tags -> string
val tags_move_to_next : tags -> unit
val tags_destroy : tags -> unit


