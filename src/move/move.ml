open Core
open Notmuch
open Unsafe

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

let describe msg add del =
  let open Printf in
  let minus s = printf " - %s\n" s in
  let plus s = printf " + %s\n" s in
  if not ( add = [] && del = [] )
  then
    Message.get_tags msg
    |> String.concat ~sep:", "
    |> printf "tag: %s\n" ;
  List.iter ~f:minus del ;
  List.iter ~f:plus add

let mover ~dry_run ~verbose ~srch_str ~debug ~db ~rules =
  let module Cfg = struct
    let verbose = verbose
    let dry_run = dry_run
    let base_dir = Database.get_path db
  end
  in
  let module T = Tools.Make(Cfg) in
  let open T in
  let open Set in
  (* per message *)
  let work target msg =
    (* fold filenames of msg *)
    let init = (None, target, []) in
    let ff (_, to_add, to_delete) path =
      let mail = mail_of_filepath path in
      let mail_dir = dir_of_mail mail in
      let to_add = remove to_add mail_dir in
      let to_delete =
        if not (mem target mail_dir)
        then mail :: to_delete
        else to_delete
      in
      (Some mail, to_add, to_delete)
    in
    let mail_o, to_add, to_delete = Message.fold_filenames ~f:ff ~init msg in
    (* action *)
    let () = if debug then
      let add = to_list to_add in
      let del = to_delete |> List.map ~f:dir_of_mail in
      describe msg add del
    in
    (* This might trigger where messages don't have any filename *)
    let mail = Option.value_exn mail_o in
    (* first add .. *)
    Set.iter ~f:(cp mail) to_add ;
    (* .. then remove filenames *)
    List.iter ~f:rm to_delete
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

let with_config f =
  let open Printf in
  let open Ext.Result in
  Config.load ()
  |> and_then_pair ~fpair:(
    ( fun cfg ->
      Config.get ~section:"database" ~key:"path" cfg
      |> and_then_opt ~err:"Failed to open database" ~f:Database.open_ )
    , fun cfg ->
      Config.get ~section:"ocaml" ~key:"move_config" cfg
      |> map ~f:Rules.from_file )
  |> function
    | Error e -> eprintf "%s\n" e
    | Ok (db, rules) -> f ~db ~rules

open Cmdliner

let move =
  let doc = "Actually move messages" in
  Arg.(value & flag & info ["m"; "move"] ~doc)

let verbose =
  let doc = "Print actions" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let debug =
  let doc = "Print differences in tags" in
  Arg.(value & flag & info ["d"; "debug"] ~doc)

let srch_lst =
  let doc = "Notmuch search term to filter operation on" in
  let env = Arg.env_var "NOTMUCH_SEARCH_TERM" ~doc in
  Arg.(value & pos_all string ["path:**"] & info [] ~env ~docv:"SEARCH_TERM" ~doc)

open Term

let wrapper move verbose debug srch_lst =
  let srch_str =
    match srch_lst with
    | [] -> "*"
    | l  -> String.concat ~sep:" " l
  in
  let dry_run = not move in
  let () = if not (move || verbose || debug) then
    printf "No action specified. Doing nothing.\n"
  in
  mover ~dry_run ~debug ~verbose ~srch_str |> with_config

let main_cmd =
  let doc = "Move messages in notmuch database according to tags" in
  const wrapper $ move $ verbose $ debug $ srch_lst,
  info "move" ~doc
