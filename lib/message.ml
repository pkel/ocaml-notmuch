(* We store message id and retrieve the message as we need it *)
type t = string

exception Message_not_available of string

let get_id msg = msg

(* make unsafe message function safe *)
let lift fn id =
  let open Ctypes in
  let open Unsafe_foreign in
  let msg_ptr = allocate message_t null in
  let open Unsafe_status in
  let msg = match database_find_message db id |> check with
    | Some err -> raise Message_not_available err
    | None -> !@msg_ptr
  in
  fn msg

module U = Unsafe_message
let get_tags = lift U.get_tags
let fold_tags = lift U.fold_tags
let add_tags msg = lift U.add_tags
let remove_tags msg  = lift U.remove_tags
let set_tags msg = lift U.set_tags
let fold_filenames msg = lift U.fold_filenames
let get_filenames msg = lift U.get_filenames
(* TODO: what about exception thrown in the above functions? *)
