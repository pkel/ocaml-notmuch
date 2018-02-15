
(* val query_messages : Database.t -> string -> Messages.t *)

let query_messages db str =
  (* TODO: Check return values *)
  let q = C.query_create db str in
  C.query_search_messages q


