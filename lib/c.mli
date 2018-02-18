open Ctypes
open PosixTypes

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
type message_properties = unit ptr

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
val messages_collect_tags : messages -> tags

(** Tags iterator *)
val tags_valid : tags -> bool
val tags_get : tags -> string
val tags_move_to_next : tags -> unit
val tags_destroy : tags -> unit

(** Filenames iterator *)
val filenames_valid : filenames -> bool
val filenames_get : filenames -> string
val filenames_move_to_next : filenames -> unit
val filenames_destroy : filenames -> unit

(** Properties iterator *)
val message_properties_valid : message_properties -> bool
val message_properties_key : message_properties -> string
val message_properties_value : message_properties -> string
val message_properties_move_to_next : message_properties -> unit
val message_properties_destroy : message_properties -> unit

(** Properties key/value access *)
val message_get_property : message -> string -> string ptr -> status
val message_add_property : message -> string -> string -> status
val message_remove_property : message -> string -> string -> status
val message_remove_all_properties : message -> string -> status

(** Thread access *)
val thread_get_thread_id : thread -> string
val thread_get_total_messages :  thread -> int
val thread_get_matched_messages : thread -> int
val thread_get_toplevel_messages : thread -> messages
val thread_get_messages : thread -> messages
val thread_get_authors : thread -> string
val thread_get_subject : thread -> string
val thread_get_oldest_date : thread -> time_t
val thread_get_newest_date : thread -> time_t
val thread_get_tags : thread -> tags
val thread_destroy : thread -> unit

(** Message access *)
val message_get_message_id : message -> string
val message_get_thread_id : message -> string
val message_get_replies : message -> messages
val message_get_filename : message -> string
val message_get_filenames : message -> filenames
val message_get_flag : message -> message_flag -> bool
val message_set_flag : message -> message_flag -> bool -> unit
val message_get_date : message -> time_t
val message_get_header : message -> string -> string
val message_get_tags : message -> tags
val message_add_tag : message -> string -> status
val message_remove_tag : message -> string -> status
val message_remove_all_tags : message -> status
val message_maildir_flags_to_tags : message -> status
val message_tags_to_maildir_flags : message -> status
val message_freeze : message -> status
val message_thaw : message -> status
val message_destroy : message -> unit

(** Missing:
  * directory_*
  * config_*
  * database_*
  *)

