(** Copy & delete messages in a maildir tree *)
open Core

module type M = sig
  type subdir = string
  type dir = string
  type mail

  exception InvalidFilepath of string
  exception InvalidDirectory of string

  val mail_of_filepath: string -> mail
  val dir_of_mail: mail -> dir

  (* val count: dir -> int *)
  (* val ls: dir -> subdir list * mail list *)
  val cp: mail -> dir -> unit
  (* val mv: mail -> dir -> unit *)
  val rm: mail -> unit
  (* val rmdir: dir -> unit *)
end

module type Cfg = sig
  val base_dir : string
  val dry_run : bool
  val verbose : bool
end

module Make(Cfg: Cfg) : M = struct
  open FilePath

  exception InvalidFilepath of string
  exception InvalidDirectory of string

  type subdir = string
  type dir = string

  type state =
    | New
    | Cur
    | Tmp

  type mail =
    { dir : string
    ; filename : string
    ; state : state
    }

  let make_absolute = make_absolute Cfg.base_dir
  let make_relative = make_relative Cfg.base_dir

  let state_to_string = function
    | Cur -> "cur"
    | Tmp -> "tmp"
    | New -> "new"

  let check_dir str =
    match basename str |> String.lowercase with
    | "cur"
    | "tmp"
    | "new" -> raise (InvalidDirectory str)
    | _ -> ()

  let mail_of_filepath str =
    let filename = basename str in
    let tmp = dirname str in
    let state = match basename tmp with
      | "cur" -> Cur
      | "tmp" -> Tmp
      | "new" -> New
      | _ -> raise (InvalidFilepath str)
    in
    let dir = dirname tmp |> make_relative in
    { dir ; filename ; state}

  let dir_of_mail { dir; _ } = dir

  let filepath_of_mail { filename; state; dir } =
    make_filename [ dir; state_to_string state ; filename ]
      |> make_absolute

  let make_maildir dir =
    if Cfg.verbose then
      printf "mkdir %s\n" dir ;
    let open FileUtil in
    [ Cur; Tmp; New ]
      |> List.map  ~f:state_to_string
      |> List.iter ~f:(fun x ->
          make_filename [dir; x] |>
          make_absolute |>
          mkdir ~parent:true
      )

  let assure_dir dir =
    let open FileUtil in
    let dir = make_absolute dir in
    if not (test (Is_dir) dir) then
      make_maildir dir

  let cp mail dst_dir =
    let () = check_dir dst_dir in
    let src = filepath_of_mail mail in
    let dst = filepath_of_mail { mail with dir = dst_dir } |> dirname in
    if Cfg.verbose then
      printf "cp %s %s\n" (make_relative src) (make_relative dst)
    else () ;
    if not Cfg.dry_run then begin
      assure_dir dst ;
      FileUtil.cp [src] dst
    end

  let rm mail =
    let src = filepath_of_mail mail in
    if Cfg.verbose then
      printf "rm %s\n" (make_relative src)
    else () ;
    if not Cfg.dry_run then
      FileUtil.rm [src]

end
