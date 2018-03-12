open Core
open Notmuch

type tag = string
type folder = string
type selector = tag list

type rule =
  | Add of folder
  | Fst of (selector * rule) list
  | All of (selector * rule) list

let default = []
let tag s = [s]
let tags l = l

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
        "order"; "invoice"; "masterthesis"; "booking" ] ]
  ]

let default_folder = "hetzner/default"

module Set = Set.Make(String)

let folders rule msg =
  let open Set in
  let msg_tags = Message.get_tags msg |> of_list in
  let rec h acc rule =
    let f (selector, rule) =
      (* apply rule if selector triggers *)
      if is_subset (of_list selector) ~of_:msg_tags
      then Some (h acc rule)
      else None
    in
    match rule with
    | Fst lst -> List.find_map ~f lst |> Option.value ~default:acc
    | All lst -> List.filter_map ~f lst |> List.fold ~init:acc ~f:union
    | Add folder -> add acc folder
  in
  match h empty rule |> to_list with
  | [] -> [ default_folder ]
  | lst -> lst

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
  (* TODO: This can fail *)
  FileUtil.cp [filepath_of_location src] dst

let remove src =
  (* TODO: This can fail *)
  FileUtil.rm [filepath_of_location src]

let mover ~move ~verbose ~srch_str db =
  let to_loc = location_of_filepath ~base:(Database.get_path db) in
  let f msg =
    (* parse filepaths *)
    let locations =
      Message.get_filenames msg |> List.map ~f:to_loc
    in
    (* calculate diff *)
    let was = List.map ~f:(fun x -> x.folder) locations in
    let will = folders folder_rule msg in
    let equal a b = if Pervasives.compare a b = 0 then true else false in
    let del =
      List.filter
        ~f:(fun x -> not (List.mem ~equal will x.folder))
        locations
    in
    let add =
      List.filter
        ~f:(fun x -> not (List.mem ~equal was  x))
        will
    in
    (* action *)
    if verbose then describe msg add del ;
    if move then begin
      let src = List.nth_exn locations 0 in
      (* first add .. *)
      List.iter ~f:(copy src) add ;
      (* .. then remove filenames *)
      List.iter ~f:remove del
    end
  in
  let open Query in
  from_string srch_str |> Messages.iter ~f db

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
