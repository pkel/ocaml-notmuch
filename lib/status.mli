exception Notmuch_error of string

(* Check error code and raise Notmuch_error if neccessary *)
val throw : C.status -> unit

(* Check error code and return error message option *)
val check : C.status -> string option
