type t
type db = Database.t

val from_string : string -> t

type exclude =
  | Flag
  | True
  | False
  | All

type sort =
  | Oldest_first
  | Newest_first
  | Message_id
  | Unsorted

val set_exclude : exclude -> t -> t
val exclude_tag : string -> t -> t
val set_sort : sort -> t -> t

module type Exec = sig
  (** Elements of type tmp are temporary and
   *  should NOT be returned by functions f
   *)
  type tmp

  val count  : db -> t -> int
  val fold   : db -> t -> init:'acc -> f:('acc -> tmp -> 'acc) -> 'acc
  val map    : db -> t -> f:(tmp -> 'a) -> 'a list
  val iter   : db -> t -> f:(tmp -> unit) -> unit
end

module Messages : Exec with type tmp := C.message
module Threads  : Exec with type tmp := C.thread

