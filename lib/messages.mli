(** opaque handler for multiple messages *)
type t
type m = Message.t

val fold: t -> init:'acc -> f:('acc -> m -> 'acc) -> 'acc
val map:  t -> f:(m -> 'a) -> 'a list
val iter: t -> f:(m -> unit) -> unit

val destroy: t -> unit
