exception Notmuch_error of string

module C = Unsafe_foreign

let describe = C.status_to_string

(* Check error code and raise Notmuch_error if neccessary *)
(* val throw : C.status -> unit *)
let throw = function
  | 0 -> ()
  | s -> Notmuch_error (describe s) |> raise

(* Check error code and return error message option *)
(* val check : C.status -> string option *)
let check = function
  | 0 -> None
  | s -> Some (describe s)
