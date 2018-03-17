open Core
open Notmuch
open Rules

let default = []
let tag s = [s]
let tags l = l

let tag_folder s f = Filter ([s], Folder f)

let folder_per_tag ?(all=false) ?(prefix="") tags =
  let open Rules in
  let f s = tag_folder s (prefix ^ s) in
  let rules = List.map ~f tags in
  if all then All rules else First rules

module Set = Set.Make(String)

let tag_set_of_message msg =
  let f = Set.add in
  let init = Set.empty in
  Message.fold_tags ~f ~init msg

let folders rule msg =
  let open Set in
  let msg_tags = tag_set_of_message msg in
  let rec h folders rule =
    let open Rules in
    match rule with
    | Folder f -> Some (add folders f)
    | FolderPerTag prefix -> Some(folders)
    | Filter (sel, rule) ->
        if of_list sel |> is_subset ~of_:msg_tags
        then h folders rule
        else None
    | First lst -> List.find_map ~f:(h folders) lst
    | All   lst ->
        match List.filter_map ~f:(h empty) lst with
        | [] -> None
        | l  -> Some (List.fold ~init:folders ~f:union l)
  in
  h empty rule

type mds =
  | Cur
  | New
  | Tmp

let mds_to_string = function
  | Cur -> "cur"
  | Tmp -> "tmp"
  | New -> "new"

type location =
  { base: string
  ; folder: string
  ; state: mds
  ; file: string
  }

let relative = FilePath.make_relative
let absolute = FilePath.make_absolute

let location_of_filepath ~base fp =
  let file = FilePath.basename fp in
  let dir = FilePath.dirname fp in
  let folder = FilePath.dirname dir |> relative base in
  let state =
    match FilePath.basename dir with
    | "cur" -> Cur
    | "tmp" -> Tmp
    | "new" -> New
    | _ -> Printf.sprintf "Invalid FilePath: %s" fp |> failwith
  in
  { base; file; state; folder }

let filepath_of_location { base; file; state; folder } =
  let d = mds_to_string state in
  FilePath.make_filename [ folder; d; file ]
    |> absolute base

let describe msg add del =
  let open Printf in
  let minus loc = printf " - %s\n" loc.folder in
  let plus s = printf " + %s\n" s in
  if not ( add = [] && del = [] )
  then
    Message.get_tags msg
    |> String.concat ~sep:", "
    |> printf "tag: %s\n" ;
  List.iter ~f:minus del ;
  List.iter ~f:plus add

let copy src dst =
  let dst = FilePath.make_filename [ src.base; dst; mds_to_string src.state ] in
  let src = filepath_of_location src in
  printf "cp %s %s\n" src dst
  (* TODO: This can fail *)
  (* FileUtil.cp src dst *)

let rm src =
  let src = filepath_of_location src in
  printf "rm %s\n" src
  (* TODO: This can fail *)
  (* FileUtil.rm [filepath_of_location src] *)

let mover ~move ~verbose ~srch_str db =
  let open Set in
  let cfg_loc = "tools/move/cfg.scm" in
  let to_location = location_of_filepath ~base:(Database.get_path db) in
  let rules = Rules.from_file cfg_loc in
  (* per message *)
  let work target msg =
    (* fold filenames of msg *)
    let init = (None, target, []) in
    let ff (_, to_add, to_delete) path =
      let loc = to_location path in
      let to_add = remove to_add loc.folder in
      let to_delete =
        if not (mem target loc.folder)
        then loc :: to_delete
        else to_delete
      in
      (Some loc, to_add, to_delete)
    in
    let fp, to_add, to_delete = Message.fold_filenames ~f:ff ~init msg in
    (* action *)
    if verbose then describe msg (to_list to_add) to_delete ;
    if move then begin
      (* This might trigger where messages don't have any filename *)
      let src = Option.value_exn fp in
      (* first add .. *)
      Set.iter ~f:(copy src) to_add ;
      (* .. then remove filenames *)
      List.iter ~f:rm to_delete
    end
  in
  (* fold messages and count rule failures *)
  let f acc msg =
    match folders rules msg with
    | None -> acc + 1
    | Some on -> work on msg ; acc
  in
  let open Query in
  let nfails = from_string srch_str |> Messages.fold ~init:0 ~f db in
  if nfails > 0 then
    Printf.eprintf
      "WARNING: Insufficient rules. Ignored %d unmatched messages.\n"
      nfails

let with_db f =
  let open Printf in
  let open Ext.Result in
  fun () -> Config.load ()
  |> and_then ~f:(Config.get ~section:"database" ~key:"path")
  |> and_then_opt ~err:"Failed to open database" ~f:Database.open_
  |> function
    | Error e -> eprintf "%s\n" e
    | Ok db -> f db

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Move messages in notmuch database according to tags"
    [%map_open
      let move  = flag "move"  no_arg ~doc:"Move instead of dry run"
      and quiet = flag "quiet" no_arg ~doc:"Don't print actions"
      and srch_lst = anon (sequence ("search-term" %: string))
      in
      let srch_str =
        match srch_lst with
        | [] -> "*"
        | l  -> String.concat ~sep:" " l
      in
      let verbose = not quiet in
      mover ~move ~verbose ~srch_str |> with_db
    ] |> Command.run
