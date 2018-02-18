type t = C.message

let throw = Status.throw

(* val get_tags : t -> string list *)
let get_tags msg =
  let tags = C.message_get_tags msg in
  let f x = x in
  Iterate.Tags.map tags ~f

(* val add_tags : t -> string list -> unit *)
let add_tags msg lst =
  let f tag = C.message_add_tag msg tag |> throw in
  List.iter f lst

(* val remove_tags : t -> string list -> unit *)
let remove_tags msg lst =
  let f tag = C.message_remove_tag msg tag |> throw in
  List.iter f lst

(* val set_tags : t -> string list -> unit *)
let set_tags msg lst =
  (* safe setting of tags as discussed in notmuch.h *)
  C.message_freeze msg |> throw ;
  C.message_remove_all_tags msg |> throw ;
  add_tags msg lst ;
  C.message_thaw msg |> throw
