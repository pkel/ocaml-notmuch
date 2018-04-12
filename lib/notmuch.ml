(** Access the notmuch configuration file *)
module Config = Config

module Unsafe = struct
  (** Unsafe access to C objects
   *
   *  Messages and other objects might mutate or get deallocated. Unexpected
   *  behaviour and segfaults lurking!
   *
   *  This was useful for development but should not be used anywhere.
   *)

  module Database = Unsafe_database
  module Query    = Unsafe_query
  module Message  = Unsafe_message
end

module Ext = struct
  (** Helpers for the option type *)
  module Option = Ext_option
  (** Helpers for the result type *)
  module Result = Ext_result
end
