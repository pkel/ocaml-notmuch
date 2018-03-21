module type M = sig
  type ptr
  type el

  val fold : ptr -> init:'acc -> f:('acc -> el -> 'acc) -> 'acc
  val map : ptr -> f:(el -> 'a) -> 'a list
  val iter : ptr -> f:(el -> unit) -> unit
  val stream : ?finalize:(unit -> unit)  -> ptr -> el Lwt_stream.t
end

type key_value = string * string

module Messages : M with type ptr := C.messages and type el := C.message
module Threads : M with type ptr := C.threads and type el := C.thread
module Properties : M
  with type ptr := C.message_properties
  and  type el := key_value
module Tags : M with type ptr := C.tags and type el := string
module Filenames : M with type ptr := C.filenames and type el := string

