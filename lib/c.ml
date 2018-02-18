open Ctypes
open PosixTypes
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
type message_properties = unit ptr

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
let message_properties_t : message_properties typ = ptr void

let database_open =
  foreign "notmuch_database_open"
    (string @-> database_mode_t @-> ptr database_t @-> returning status_t)

let database_close =
  foreign "notmuch_database_close"
    (database_t @-> returning status_t)

let database_get_revision =
  foreign "notmuch_database_get_revision"
    (database_t @-> ptr string @-> returning ulong)

let database_get_all_tags =
  foreign "notmuch_database_get_all_tags"
    (database_t @-> returning tags_t)

let query_create =
  foreign "notmuch_query_create"
    (database_t @-> string @-> returning query_t)

let query_get_query_string =
  foreign "notmuch_query_get_query_string"
    (query_t @-> returning string)

let query_get_database =
  foreign "notmuch_query_get_database"
    (query_t @-> returning database_t)

let query_set_omit_excluded =
  foreign "notmuch_query_set_omit_excluded"
    (query_t @-> exlude_t @-> returning void)

let query_set_sort =
  foreign "notmuch_query_set_sort"
    (query_t @-> sort_t @-> returning void)

let query_get_sort =
  foreign "notmuch_query_get_sort"
    (query_t @-> returning sort_t)

let query_add_tag_exclude =
  foreign "notmuch_query_add_tag_exclude"
    (query_t @-> string @-> returning status_t)

let query_search_threads =
  foreign "notmuch_query_search_threads"
    (query_t @-> ptr threads_t @-> returning status_t)

let query_search_messages =
  foreign "notmuch_query_search_messages"
    (query_t @-> ptr messages_t @-> returning status_t)

let query_destroy =
  foreign "notmuch_query_destroy"
    (query_t @-> returning void)

let query_count_threads =
  foreign "notmuch_query_count_threads"
    (query_t @-> ptr uint @-> returning status_t)

let query_count_messages =
  foreign "notmuch_query_count_messages"
    (query_t @-> ptr uint @-> returning status_t)

let threads_valid =
  foreign "notmuch_threads_valid"
    (threads_t @-> returning bool)

let threads_get =
  foreign "notmuch_threads_get"
    (threads_t @-> returning thread_t)

let threads_move_to_next =
  foreign "notmuch_threads_move_to_next"
    (threads_t @-> returning void)

let threads_destroy =
  foreign "notmuch_threads_destroy"
    (threads_t @-> returning void)

let messages_valid =
  foreign "notmuch_messages_valid"
    (messages_t @-> returning bool)

let messages_get =
  foreign "notmuch_messages_get"
    (messages_t @-> returning message_t)

let messages_move_to_next =
  foreign "notmuch_messages_move_to_next"
    (messages_t @-> returning void)

let messages_destroy =
  foreign "notmuch_messages_destroy"
    (messages_t @-> returning void)

let messages_collect_tags =
  foreign "notmuch_messages_collect_tags"
    (messages_t @-> returning tags_t)

let tags_valid =
  foreign "notmuch_tags_valid"
    (tags_t @-> returning bool)

let tags_get =
  foreign "notmuch_tags_get"
    (tags_t @-> returning string)

let tags_move_to_next =
  foreign "notmuch_tags_move_to_next"
    (tags_t @-> returning void)

let tags_destroy =
  foreign "notmuch_tags_destroy"
    (tags_t @-> returning void)

(** Filenames iterator *)
let filenames_valid =
  foreign "notmuch_filenames_valid"
    (filenames_t @-> returning bool)

let filenames_get =
  foreign "notmuch_filenames_get"
    (filenames_t @-> returning string)

let filenames_move_to_next =
  foreign "notmuch_filenames_move_to_next"
    (filenames_t @-> returning void)

let filenames_destroy =
  foreign "notmuch_filenames_destroy"
    (filenames_t @-> returning void)


(** Properties iterator *)
let message_properties_valid =
  foreign "notmuch_message_properties_valid"
    (message_properties_t @-> returning bool)

let message_properties_key =
  foreign "notmuch_message_properties_key"
    (message_properties_t @-> returning string)

let message_properties_value =
  foreign "notmuch_message_properties_value"
    (message_properties_t @-> returning string)

let message_properties_move_to_next =
  foreign "notmuch_message_properties_move_to_next"
    (message_properties_t @-> returning void)

let message_properties_destroy =
  foreign "notmuch_message_properties_destroy"
    (message_properties_t @-> returning void)


(** Properties key/value access *)
let message_get_property =
  foreign "notmuch_message_get_property"
    (message_t @-> string @-> ptr string @-> returning status_t)

let message_add_property =
  foreign "notmuch_message_add_property"
    (message_t @-> string @-> string @-> returning status_t)

let message_remove_property =
  foreign "notmuch_message_remove_property"
    (message_t @-> string @-> string @-> returning status_t)

let message_remove_all_properties =
  foreign "notmuch_message_remove_all_properties"
    (message_t @-> string @-> returning status_t)


(** Thread access *)
let thread_get_thread_id =
  foreign "notmuch_thread_get_thread_id"
    (thread_t @-> returning string)

let thread_get_total_messages =
  foreign "notmuch_thread_get_total_messages"
    (thread_t @-> returning int)

let thread_get_matched_messages =
  foreign "notmuch_thread_get_matched_messages"
    (thread_t @-> returning int)

let thread_get_toplevel_messages =
  foreign "notmuch_thread_get_toplevel_messages"
    (thread_t @-> returning messages_t)

let thread_get_messages =
  foreign "notmuch_thread_get_messages"
    (thread_t @-> returning messages_t)

let thread_get_authors =
  foreign "notmuch_thread_get_authors"
    (thread_t @-> returning string)

let thread_get_subject =
  foreign "notmuch_thread_get_subject"
    (thread_t @-> returning string)

let thread_get_oldest_date =
  foreign "notmuch_thread_get_oldest_date"
    (thread_t @-> returning time_t)

let thread_get_newest_date =
  foreign "notmuch_thread_get_newest_date"
    (thread_t @-> returning time_t)

let thread_get_tags =
  foreign "notmuch_thread_get_tags"
    (thread_t @-> returning tags_t)

let thread_destroy =
  foreign "notmuch_thread_destroy"
    (thread_t @-> returning void)


(** Message access *)
let message_get_message_id =
  foreign "notmuch_message_get_message_id"
    (message_t @-> returning string)

let message_get_thread_id =
  foreign "notmuch_message_get_thread_id"
    (message_t @-> returning string)

let message_get_replies =
  foreign "notmuch_message_get_replies"
    (message_t @-> returning messages_t)

let message_get_filename =
  foreign "notmuch_message_get_filename"
    (message_t @-> returning string)

let message_get_filenames =
  foreign "notmuch_message_get_filenames"
    (message_t @-> returning filenames_t)

let message_get_flag =
  foreign "notmuch_message_get_flag"
    (message_t @-> message_flag_t @-> returning bool)

let message_set_flag =
  foreign "notmuch_message_set_flag"
    (message_t @-> message_flag_t @-> bool @-> returning void)

let message_get_date =
  foreign "notmuch_message_get_date"
    (message_t @-> returning time_t)

let message_get_header =
  foreign "notmuch_message_get_header"
    (message_t @-> string @-> returning string)

let message_get_tags =
  foreign "notmuch_message_get_tags"
    (message_t @-> returning tags_t)

let message_add_tag =
  foreign "notmuch_message_add_tag"
    (message_t @-> string @-> returning status_t)

let message_remove_tag =
  foreign "notmuch_message_remove_tag"
    (message_t @-> string @-> returning status_t)

let message_remove_all_tags =
  foreign "notmuch_message_remove_all_tags"
    (message_t @-> returning status_t)

let message_maildir_flags_to_tags =
  foreign "notmuch_message_maildir_flags_to_tags"
    (message_t @-> returning status_t)

let message_tags_to_maildir_flags =
  foreign "notmuch_message_tags_to_maildir_flags"
    (message_t @-> returning status_t)

let message_freeze =
  foreign "notmuch_message_freeze"
    (message_t @-> returning status_t)

let message_thaw =
  foreign "notmuch_message_thaw"
    (message_t @-> returning status_t)

let message_destroy =
  foreign "notmuch_message_destroy"
    (message_t @-> returning void)
