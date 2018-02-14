open Ctypes
open Foreign

let bool_t = bool

type status = int
let status_t = int

type tags = unit ptr
let tags_t : tags typ = ptr void

type database = unit ptr
let database_t : database typ = ptr void

type database_mode =  int
let database_mode_t =  int

let database_open =
  foreign "notmuch_database_open"
    (string @-> database_mode_t @-> ptr database_t @-> returning status_t)

let database_close =
  foreign "notmuch_database_close"
    (database_t @-> returning status_t)

let database_get_revision =
  foreign
    "notmuch_database_get_revision"
    (database_t @-> ptr string @-> returning ulong)

let database_get_all_tags =
  foreign
    "notmuch_database_get_all_tags"
    (database_t @-> returning tags_t)

let tags_valid =
  foreign
    "notmuch_tags_valid"
    (tags_t @-> returning bool_t)

let tags_get =
  foreign
    "notmuch_tags_get"
    (tags_t @-> returning string)

let tags_move_to_next =
  foreign
    "notmuch_tags_move_to_next"
    (tags_t @-> returning void)

let tags_destroy =
  foreign
    "notmuch_tags_destroy"
    (tags_t @-> returning void)
