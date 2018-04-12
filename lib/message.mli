type t

val get_id : t -> string

(** Messages are represented by their id. On access, are located within
 *  the notmuch database. The following exception is raised, if the message
 *  got removed from the database between object creation and usage.
 *
 *  All of the following functions may raise this exception. Keep message
 *  object lifetimes short and don't remove messages from notmuch database
 *  in parallel to avoid this.
 *
 *  The C api's status message is attached for debugging.
 *)
exception Message_gone of string

(** Tags *)
val fold_tags   : t -> init:'a -> f:('a -> string -> 'a) -> 'a
val get_tags    : t -> string list
val set_tags    : t -> string list -> unit
val add_tags    : t -> string list -> unit
val remove_tags : t -> string list -> unit

(** Filenames *)
val fold_filenames : t -> init:'a -> f:('a -> string -> 'a) -> 'a
val get_filenames  : t -> string list
