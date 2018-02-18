module type M = sig
  type ptr
  type el

  val fold : ptr -> init:'acc -> f:('acc -> el -> 'acc) -> 'acc
  val map : ptr -> f:(el -> 'a) -> 'a list
  val iter : ptr -> f:(el -> unit) -> unit
end

module type In = sig
  type ptr
  type el

  val valid   : ptr -> bool
  val get     : ptr -> el
  val move    : ptr -> unit
  val destroy : ptr -> unit
end

module F (I : In) : (M with type ptr := I.ptr and type el := I.el) = struct
  let fold ptr ~init ~f =
    (* iterate and build list *)
    let rec h acc =
      if I.valid ptr then
        let el = I.get ptr in
        I.move ptr ;
        h (f acc el)
      else
        acc
    in
    let res = h init in
    (* destroy c iterator object and thereby all elements
     * everyting in the c world will be lost from here on
     *)
    I.destroy ptr ;
    res

  let map ptr ~f =
    let init = [] in
    let f acc e = (f e) :: acc in
    fold ptr ~init ~f |> List.rev

  let iter ptr ~f =
    let init = () in
    let f acc e = f e in
    fold ptr ~init ~f
end

module Mes : In
  with type ptr = C.messages
  and  type el = C.message
  = struct

  type ptr = C.messages
  type el = C.message

  let valid = C.messages_valid
  let get = C.messages_get
  let move = C.messages_move_to_next
  let destroy = C.messages_destroy
end
module Messages = F(Mes)

module Thr : In
  with type ptr = C.threads
  and  type el = C.thread
  = struct

  type ptr = C.threads
  type el = C.thread

  let valid = C.threads_valid
  let get = C.threads_get
  let move = C.threads_move_to_next
  let destroy = C.threads_destroy
end
module Threads = F(Thr)

type key_value = string * string

module Pro : In
  with type ptr = C.message_properties
  and  type el = key_value
  = struct

  type ptr = C.message_properties
  type el = key_value

  let valid = C.message_properties_valid
  let get ptr =
    ( C.message_properties_key ptr
    , C.message_properties_value ptr )
  let move = C.message_properties_move_to_next
  let destroy = C.message_properties_destroy
end
module Properties = F(Pro)

module Fil : In
  with type ptr = C.filenames
  and  type el = string
  = struct

  type ptr = C.filenames
  type el = string

  let valid = C.filenames_valid
  let get = C.filenames_get
  let move = C.filenames_move_to_next
  let destroy = C.filenames_destroy
end
module Filenames = F(Fil)

module Tag : In
  with type ptr = C.tags
  and  type el = string
  = struct

  type ptr = C.tags
  type el = string

  let valid = C.tags_valid
  let get = C.tags_get
  let move = C.tags_move_to_next
  let destroy = C.tags_destroy
end
module Tags = F(Tag)
