open Core
open Notmuch

let db_loc = "/home/patrik/mail"

let get_db () =
  match Database.open_ db_loc with
  | None -> failwith "Could not open database"
  | Some db -> db

type tag = string
type folder = string
type selector = tag list

type rule =
  | Add of folder
  | Fst of (selector * rule) list
  | All of (selector * rule) list

let default = []
let tag s = [s]

let folder_per_tag ?(all=false) ?(prefix="") tags =
  let f s = tag s , Add (prefix ^ s) in
  let rules = List.map ~f tags in
  if all then All rules else Fst rules

let folder_rule : rule =
  All [
    tag "todo" , Add "hetzner/todo" ;
    default    , Fst [
      tag "spam"  , Add "hetzner/spambucket" ;
      tag "trash" , Add "hetzner/Trash" ;
      tag "inbox" , Fst [
        tag "uibk"    , Add "uibk/INBOX" ;
        tag "student" , Add "uibk-student/INBOX" ;
        default       , Add "hetzner/INBOX" ] ;
      tag "sent"  , All [
        tag "uibk"    , Add "uibk/Sent" ;
        tag "student" , Add "uibk-student/Sent" ;
        tag "private" , Add "hetzner/Sent" ] ;
      default     , folder_per_tag  ~all:true ~prefix:"hetzner/" [
        "notif"; "order"; "invoice"; "masterthesis"; "booking" ] ]
  ]

let default_folder = "hetzner/default"

module Set = Set.Make(String)

let tag_set_of_message msg =
  let f = Set.add in
  let init = Set.empty in
  Message.fold_tags ~f ~init msg

let folders rule msg =
  let open Set in
  let msg_tags = tag_set_of_message msg in
  let rec h acc rule =
    let f (selector, rule) =
      (* apply rule if selector triggers *)
      if of_list selector |> is_subset ~of_:msg_tags
      then Some (h acc rule)
      else None
    in
    match rule with
    | Fst lst -> List.find_map ~f lst |> Option.value ~default:acc
    | All lst -> List.filter_map ~f lst |> List.fold ~init:acc ~f:union
    | Add folder -> add acc folder
  in
  let ret = h empty rule in
  if is_empty ret then singleton default_folder else ret

type mds =
  | Cur
  | New
  | Tmp

let mds_to_string = function
  | Cur -> "cur"
  | Tmp -> "tmp"
  | New -> "new"

type location =
  { folder: string
  ; state: mds
  ; file: string
  }

let relative = FilePath.make_relative db_loc
let absolute = FilePath.make_absolute db_loc

let location_of_filepath fp =
  let file = FilePath.basename fp in
  let dir = FilePath.dirname fp in
  let folder = FilePath.dirname dir |> relative in
  let state =
    match FilePath.basename dir with
    | "cur" -> Cur
    | "tmp" -> Tmp
    | "new" -> New
    | _ -> Printf.sprintf "Invalid FilePath: %s" fp |> failwith
  in
  { file; state; folder }

let filepath_of_location { file; state; folder } =
  let d = mds_to_string state in
  FilePath.make_filename [ folder; d; file ]
    |> absolute

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
  let dst = FilePath.make_filename [ dst; mds_to_string src.state ] in
  (* TODO: This can fail *)
  FileUtil.cp [filepath_of_location src] dst

let rm src =
  (* TODO: This can fail *)
  FileUtil.rm [filepath_of_location src]

let mover move verbose query =
  let open Set in
  let db = get_db () in
  (* per message *)
  let f msg =
    let target = folders folder_rule msg in
    (* fold filenames of msg *)
    let init = (None, target, []) in
    let ff (_, to_add, to_delete) path =
      let loc = location_of_filepath path in
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
  let open Query in
  from_string query |> Messages.iter ~f db

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Move messages in notmuch database according to tags"
    [%map_open
      let move  = flag "move"  no_arg ~doc:"Move instead of dry run"
      and quiet = flag "quiet" no_arg ~doc:"Don't print actions"
      and srch_lst = anon (sequence ("search-term" %: string)) in
      fun () ->
        let srch_str =
          match srch_lst with
          | [] -> "*"
          | l  -> String.concat ~sep:" " l
        in
        mover move (not quiet) srch_str
    ] |> Command.run
