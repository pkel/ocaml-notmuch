open Ctypes
open Foreign

type t = unit ptr
let t_t : t typ = ptr void

(* open database at path *)
let open_ ?(write=false) path =
  let db_ptr = allocate t_t null in
  let f =
    foreign
      "notmuch_database_open"
      (string @-> int @-> ptr t_t @-> returning int)
  in
  let mode = if write then 1 else 0 in
  match f path mode db_ptr with
  | 0 -> Some (!@ db_ptr)
  | _ -> None

(* commit changes and close database *)
let close db =
  let f = foreign "notmuch_database_close" (t_t @-> returning int) in
  match f db with
  | 0 -> ()
  | _ -> raise (Failure "could not close db")


(* uuid * rev number *)
type revision = string * Unsigned.ulong

let get_revision db =
  let uuid_ptr = allocate string "" in
  let f = foreign
    "notmuch_database_get_revision"
    (t_t @-> ptr string @-> returning ulong)
  in
  let rev = f db uuid_ptr in
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

