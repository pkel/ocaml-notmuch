open Core

type tag = string [@@deriving sexp]
type selector = tag list [@@deriving sexp]

type t =
  | Folder of string
  | Filter of selector * t
  | All    of t list
  | First  of t list
  | FolderPerTag of string
  [@@deriving sexp]

(* val from_file : string -> t *)
let from_file fp =
  Sexplib.Sexp.load_sexp fp |> t_of_sexp

