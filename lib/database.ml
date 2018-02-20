open Ctypes

type t = C.database
let typ : t typ = C.database_t

(* open database at path *)
let open_ ?(write=false) path =
  let db_ptr = allocate typ null in
  let mode = if write then 1 else 0 in
  match C.database_open path mode db_ptr with
  | 0 -> Some (!@ db_ptr)
  | _ -> None

(* commit changes and close database *)
let close db =
  match C.database_close db with
  | 0 -> ()
  | _ -> raise (Failure "could not close db")

(* uuid * rev number *)
type revision = string * Unsigned.ulong

let get_revision db =
  let uuid_ptr = allocate string "" in
  let rev = C.database_get_revision db uuid_ptr in
  (!@ uuid_ptr , rev)

type rev_comp_result =
  | Newer
  | Older
  | Same
  | Incompatible

let rev_compare (ua,ra) (ub, rb) =
  let open Pervasives in
  match compare ua ub with
  | 0 -> begin
    let d = Unsigned.ULong.compare ra rb in
    if d > 0 then Older else if d < 0 then Newer else Same
  end
  | _ -> Incompatible

let string_of_revision (uuid , rev) =
  let revs = Unsigned.ULong.to_string rev in
  Printf.sprintf "%s | %s" uuid revs

let revision_of_string str =
  try
    let uuid, revs = Scanf.sscanf str "%s | %s" (fun l r -> l, r) in
    Some ( uuid , Unsigned.ULong.of_string revs )
  with _ -> None

(* val uuid_of_revision : revision -> string *)
let uuid_of_revision (uuid,_) =  uuid

(* val nr_of_revision   : revision -> string *)
let nr_of_revision (_, nr) = Unsigned.ULong.to_string nr

let get_all_tags db =
  (* get c tags object *)
  let r = C.database_get_all_tags db in
  (* check return value *)
  if r == null then raise(Failure "database_get_all_tags") else () ;
  (* iterate and build list *)
  let rec f acc =
    if C.tags_valid r then
      let tag = C.tags_get r in
      C.tags_move_to_next r ;
      f (tag :: acc)
    else
      acc
  in
  let tags = f [] in
  (* destroy c tags object *)
  C.tags_destroy r ;
  (* return list *)
  List.rev tags
