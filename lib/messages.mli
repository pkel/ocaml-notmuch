(** opaque handler for multiple messages *)
type t = C.messages
type m = C.message

val fold: t -> init:'acc -> f:('acc -> m -> 'acc) -> 'acc
val map:  t -> f:(m -> 'a) -> 'a list
val iter: t -> f:(m -> unit) -> unit

val destroy: t -> unit
