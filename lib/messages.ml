type t = C.messages
type m = C.message

(* val fold: t -> init:'acc -> f:('acc -> m -> 'acc) -> 'acc *)
let fold t ~init ~f =
  (* iterate and build list *)
  let rec h acc =
    if C.messages_valid t then
      let el = C.messages_get t in
      C.messages_move_to_next t ;
      h (f acc el)
    else
      acc
  in
  let messages = h init in
  (* destroy c messages object *)
  C.messages_destroy t ;
  messages

(* val map:  t -> f:(m -> 'a) -> 'a list *)
let map t ~f =
  let init = [] in
  let f acc e = (f e) :: acc in
  fold ~init ~f t |> List.rev

(* val iter: t -> f:(m -> unit) -> unit *)
let iter t ~f =
  let init = () in
  let f acc e = f e in
  fold ~init ~f t

(* val destroy: t -> unit *)
let destroy t =
  C.messages_destroy t
