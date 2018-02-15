open Ctypes
open Foreign

type status             = int (* enum *)
type database           = unit ptr
type query              = unit ptr
type threads            = unit ptr
type thread             = unit ptr
type messages           = unit ptr
type message            = unit ptr
type tags               = unit ptr
type directory          = unit ptr
type filenames          = unit ptr
type config_list        = unit ptr
type database_mode      = int (* enum *)
type sort               = int (* enum *)
type exlude             = int (* enum *)
type message_flag       = int (* enum *)
type message_properties = int (* enum *)

let status_t             : status             typ = int
let database_t           : database           typ = ptr void
let query_t              : query              typ = ptr void
let threads_t            : threads            typ = ptr void
let thread_t             : thread             typ = ptr void
let messages_t           : messages           typ = ptr void
let message_t            : message            typ = ptr void
let tags_t               : tags               typ = ptr void
let directory_t          : directory          typ = ptr void
let filenames_t          : filenames          typ = ptr void
let config_list_t        : config_list        typ = ptr void
let database_mode_t      : database_mode      typ = int
let sort_t               : sort               typ = int
let exlude_t             : exlude             typ = int
let message_flag_t       : message_flag       typ = int
let message_properties_t : message_properties typ = int

let database_open =
  foreign
    "notmuch_database_open"
    (string @-> database_mode_t @-> ptr database_t @-> returning status_t)

let database_close =
  foreign
    "notmuch_database_close"
    (database_t @-> returning status_t)

let database_get_revision =
  foreign
    "notmuch_database_get_revision"
    (database_t @-> ptr string @-> returning ulong)

let database_get_all_tags =
  foreign
    "notmuch_database_get_all_tags"
    (database_t @-> returning tags_t)

let query_create =
  foreign
    "notmuch_query_create"
    (database_t @-> string @-> returning query_t)

let query_get_query_string =
  foreign
    "notmuch_query_get_query_string"
    (query_t @-> returning string)

let query_get_database =
  foreign
    "notmuch_query_get_database"
    (query_t @-> returning database_t)

let query_set_omit_excluded =
  foreign
    "notmuch_query_set_omit_excluded"
    (query_t @-> exlude_t @-> returning void)

let query_set_sort =
  foreign
    "notmuch_query_set_sort"
    (query_t @-> sort_t @-> returning void)

let query_get_sort =
  foreign
    "notmuch_query_get_sort"
    (query_t @-> returning sort_t)

let query_add_tag_exclude =
  foreign
    "notmuch_query_add_tag_exclude"
    (query_t @-> string @-> returning status_t)

let query_search_threads =
  foreign
    "notmuch_query_search_threads"
    (query_t @-> ptr threads_t @-> returning status_t)

let query_search_messages =
  foreign
    "notmuch_query_search_messages"
    (query_t @-> ptr messages_t @-> returning status_t)

let query_destroy =
  foreign
    "notmuch_query_destroy"
    (query_t @-> returning void)

let query_count_threads =
  foreign
    "notmuch_query_count_threads"
    (query_t @-> ptr uint @-> returning status_t)

let query_count_messages =
  foreign
    "notmuch_query_count_messages"
    (query_t @-> ptr uint @-> returning status_t)

let threads_valid =
  foreign
    "notmuch_threads_valid"
    (threads_t @-> returning bool)

let threads_get =
  foreign
    "notmuch_threads_get"
    (threads_t @-> returning thread_t)

let threads_move_to_next =
  foreign
    "notmuch_threads_move_to_next"
    (threads_t @-> returning void)

let threads_destroy =
  foreign
    "notmuch_threads_destroy"
    (threads_t @-> returning void)

let messages_valid =
  foreign
    "notmuch_messages_valid"
    (messages_t @-> returning bool)

let messages_get =
  foreign
    "notmuch_messages_get"
    (messages_t @-> returning message_t)

let messages_move_to_next =
  foreign
    "notmuch_messages_move_to_next"
    (messages_t @-> returning void)

let messages_destroy =
  foreign
    "notmuch_messages_destroy"
    (messages_t @-> returning void)

let tags_valid =
  foreign
    "notmuch_tags_valid"
    (tags_t @-> returning bool)

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
