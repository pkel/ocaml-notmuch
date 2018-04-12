exception Notmuch_error of string

(* Check error code and raise Notmuch_error if neccessary *)
val throw : Unsafe_foreign.status -> unit

(* Check error code and return error message option *)
val check : Unsafe_foreign.status -> string option
