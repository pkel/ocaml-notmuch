open Notmuch

let syncer db store srch_lst =
  let user = Unix.getlogin () in
  let host = Unix.gethostname () in
  let module Store = TagStore.Make(struct
    open TagStore
    let author =
      { name = "notmuch-sync"
      ; mail = user ^ "@" ^ host
      }
    let location = store
  end) in
  let f acc msg =
    let id = Message.get_id msg in
    let tags = Message.get_tags msg in
    (id, tags) :: acc
  in
  let open Query in
  String.concat " " srch_lst
  |> from_string
  |> Messages.fold ~init:[] db ~f
  |> Store.set_tags
  |> Lwt_main.run

let with_db_and_store f =
  let open Printf in
  let open Ext.Result in
  fun x -> Config.load ()
  |> and_then_pair ~fpair:(
    ( fun cfg ->
      Config.get ~section:"database" ~key:"path" cfg
      |> and_then_opt ~err:"Failed to open database" ~f:Database.open_ )
    , fun cfg ->
      Config.get ~section:"ocaml" ~key:"sync_store" cfg )
  |> function
    | Error e -> eprintf "%s\n" e
    | Ok (db, store) -> f db store x

open Cmdliner

let srch_lst =
  let doc = "Overwrite the default search term" in
  let env = Arg.env_var "NOTMUCH_SEARCH_TERM" ~doc in
  let doc = "Notmuch search term to filter operation on" in
  Arg.(value & pos_all string ["path:**"] & info [] ~env ~docv:"SEARCH_TERM" ~doc)

let main_t = Term.(const (with_db_and_store syncer) $ srch_lst)

let () = Term.exit @@ Term.eval (main_t, Term.info "notmuch-sync")
