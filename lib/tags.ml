open Ctypes

type t = Api.tags

let all_tags db =
  (* get c tags object *)
  let r = Api.database_get_all_tags db in
  (* check return value *)
  if r == null then raise(Failure "database_get_all_tags") else () ;
  (* iterate and build list *)
  let rec f acc =
    if Api.tags_valid r then
      let tag = Api.tags_get r in
      Api.tags_move_to_next r ;
      f (tag :: acc)
    else
      acc
  in
  let tags = f [] in
  (* destroy c tags object *)
  Api.tags_destroy r ;
  (* return list *)
  List.rev tags
