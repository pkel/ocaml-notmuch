open Sexplib.Std

type tag = string [@@deriving sexp]
type selector = tag list [@@deriving sexp]

type t =
  | Filter of selector * t
  | Folder of string
  | All    of t list
  | First  of t list
  [@@deriving sexp]

(* val from_file : string -> t *)
let from_file fp =
  Sexplib.Sexp.load_sexp fp |> t_of_sexp

