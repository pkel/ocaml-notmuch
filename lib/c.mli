open Ctypes

(** Interface to /usr/include/notmuch.h
 *  See details there.
 *)

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

val status_t             : status             typ
val database_t           : database           typ
val query_t              : query              typ
val threads_t            : threads            typ
val thread_t             : thread             typ
val messages_t           : messages           typ
val message_t            : message            typ
val tags_t               : tags               typ
val directory_t          : directory          typ
val filenames_t          : filenames          typ
val config_list_t        : config_list        typ
val database_mode_t      : database_mode      typ
val sort_t               : sort               typ
val exlude_t             : exlude             typ
val message_flag_t       : message_flag       typ
val message_properties_t : message_properties typ

(** Database basics *)
val database_open : string -> database_mode -> database ptr -> status
val database_close : database -> status
val database_get_revision : database -> string ptr -> Unsigned.ULong.t
val database_get_all_tags : database -> tags

(** Querying *)
val query_create : database -> string -> query
val query_get_query_string : query -> string
val query_get_database : query -> database
val query_set_omit_excluded : query -> exlude -> unit
val query_set_sort : query -> sort -> unit
val query_get_sort : query -> sort
val query_add_tag_exclude : query -> string -> status
val query_search_threads : query -> threads ptr -> status
val query_search_messages : query -> messages ptr -> status
val query_destroy : query -> unit
val query_count_threads : query -> Unsigned.UInt.t ptr -> status
val query_count_messages : query -> Unsigned.UInt.t ptr -> status

(** Threads iterator *)
val threads_valid : threads -> bool
val threads_get : threads -> thread
val threads_move_to_next : threads -> unit
val threads_destroy : threads -> unit

(** Messages iterator *)
val messages_valid : messages -> bool
val messages_get : messages -> message
val messages_move_to_next : messages -> unit
val messages_destroy : messages -> unit

(** Tags iterator *)
val tags_valid : tags -> bool
val tags_get : tags -> string
val tags_move_to_next : tags -> unit
val tags_destroy : tags -> unit
