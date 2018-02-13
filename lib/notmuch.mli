(** String representation of notmuch.h errors *)
exception Notmuch_error of string

module Database = Database
module Query = Query
module Messages = Messages
module Message = Message
module Tag  = Tag
module Tags = Tags
