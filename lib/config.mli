type item =
  string * string

type section =
  string * item list

type t =
  section list

val from_file : string -> (t, string) Result.t
