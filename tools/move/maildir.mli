(** Copy & delete messages in a maildir tree *)

module type M = sig
  type subdir = string
  type dir = subdir list
  type mail

  val mail_of_filepath: string -> mail
  val dir_of_string: string -> dir
  val dir_of_mail: mail -> dir

  (* val count: dir -> int *)
  (* val ls: dir -> subdir list * mail list *)
  val cp: mail -> dir -> unit
  (* val mv: mail -> dir -> unit *)
  val rm: mail -> unit
  (* val rmdir: dir -> unit *)
end

type folder_style =
  | Dots
  | Directories

module type Cfg = sig
  val folder_style : folder_style
  val base_dir : string
  val dry_run : bool
  val verbose : bool
end

module Make (C:Cfg) : M
