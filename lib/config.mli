type item =
  string * string

type section =
  string * item list

type t =
  section list

(** Parse configuration from file *)
val from_file : string -> (t, string) result

(** Locate configuration file.
 *  Takes into account the environment variable NOTMUCH_CONFIG. *)
val locate : unit -> (string, string) result

(** Load configuration using from_file and locate *)
val load : ?custom:string -> unit -> (t, string) result

(** Get value from config: Just value = get section key *)
val get : section:string -> key:string -> t -> (string, string) result
