open Core
open Notmuch

let db_loc = "/home/patrik/mail"

let get_db () =
  match Database.open_ db_loc with
  | None -> failwith "Could not open database"
  | Some db -> db

let folders msg =
  (* TODO: This is boring *)
  [ "/home/patrik/mail/hetzner/seen/" ]

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

let location_of_filepath fp =
  let file = FilePath.basename fp in
  let dir = FilePath.dirname fp in
  let folder = FilePath.dirname dir in
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

let describe msg add del =
  let open Printf in
  let rel = FilePath.make_relative db_loc in
  let minus loc = printf " - %s\n" (rel loc.folder) in
  let plus s = printf " + %s\n" (rel s)in
  printf "id:%s\n" (Message.get_id msg) ;
  List.iter ~f:minus del ;
  List.iter ~f:plus add

let copy src dst =
  let dst = FilePath.make_filename [ dst; mds_to_string src.state ] in
  (* TODO: This can fail *)
  FileUtil.cp [filepath_of_location src] dst

let remove src =
  (* TODO: This can fail *)
  FileUtil.rm [filepath_of_location src]

let mover move verbose query =
  let db = get_db () in
  let f msg =
    (* parse filepaths *)
    let locations =
      Message.get_filenames msg |>
      List.map ~f:location_of_filepath
    in
    (* calculate diff *)
    let was = List.map ~f:(fun x -> x.folder) locations in
    let will = folders msg in
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
