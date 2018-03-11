type t = C.message

let throw = Status.throw

let get_id msg =
  C.message_get_message_id msg

(* val get_tags : t -> string list *)
let get_tags msg =
  let tags = C.message_get_tags msg in
  let f x = x in
  Iterate.Tags.map tags ~f

(* val fold_tags : t -> init:'a -> f:('a -> string -> 'a) -> 'a *)
let fold_tags msg ~init ~f =
  C.message_get_tags msg
  |> Iterate.Tags.fold ~init ~f

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

(* val fold_filenames : t -> init:'a -> f:('a -> string -> 'a) -> 'a *)
let fold_filenames msg ~init ~f =
  C.message_get_filenames msg
  |> Iterate.Filenames.fold ~init ~f

(* val get_filenames : t -> string list *)
let get_filenames msg =
  let tags = C.message_get_filenames msg in
  let f x = x in
  Iterate.Filenames.map tags ~f


