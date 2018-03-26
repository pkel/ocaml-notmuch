open Notmuch
open Lwt.Infix

let syncer ~db ~store ~remote srch_lst =
  let user = Unix.getlogin () in
  let host = Unix.gethostname () in
  let module Config = struct
    open TagStore
    let author =
      { name = "notmuch-sync"
      ; mail = user ^ "@" ^ host
      }
    let location = store
  end in
  let module Store = TagStore.Make(Config) in
  let main =
    (* Pull remote git, merge changes *)
    Store.pull remote
    >>= begin function
      | Ok () -> Lwt.return ()
      | Error e -> Lwt_io.printf "Failed to pull: %s\n" e
    end
    (* Update local git with tags *)
    >>= fun () ->
      let f msg =
        let id = Message.get_id msg in
        let tags = Message.get_tags msg in
        (id, tags)
      in
      let open Query in
      String.concat " " srch_lst
      |> from_string
      |> Messages.stream db
      |> Lwt_stream.map f
      |> Store.set_mtags_stream
    (* Push stuff to remote *)
    >>= fun () ->
      Store.push remote
    >>= begin function
      | Ok () -> Lwt.return ()
      | Error e -> Lwt_io.printf "Failed to push: %s\n" e
    end
  in
  Lwt_main.run main

let with_db_and_store f =
  let open Printf in
  let open Ext.Result in
  fun x -> Config.load ()
  |> and_then_pair ~fpair:
    ( ( fun cfg ->
      Config.get ~section:"database" ~key:"path" cfg
      |> and_then_opt ~err:"Failed to open database" ~f:Database.open_ )
    , ( fun cfg ->
      ["sync_store_local"; "sync_store_remote"]
      |> List.map (fun key -> Config.get ~section:"ocaml" ~key cfg)
      |> all )
    )
  |> function
    | Error e -> eprintf "%s\n" e
    | Ok (db , [store; remote]) -> f ~db ~store ~remote x
    | Ok _ -> assert false

open Cmdliner

let srch_lst =
  let doc = "Overwrite the default search term" in
  let env = Arg.env_var "NOTMUCH_SEARCH_TERM" ~doc in
  let doc = "Notmuch search term to filter operation on" in
  Arg.(value & pos_all string ["path:**"] & info [] ~env ~docv:"SEARCH_TERM" ~doc)

open Term

let main_t = const (with_db_and_store syncer) $ srch_lst

let () = exit @@
  eval_choice (main_t, info "onm" ~doc:"Ocaml tools for notmuch databases") [
    main_t, info "main" ~doc:"pull, write, push";
    main_t, info "read" ~doc:"update notmuch database from local store";
    main_t, info "write" ~doc:"update local store from notmuch database";
    main_t, info "pull" ~doc:"pull updates from remote store(s)";
    main_t, info "push" ~doc:"push updates to remote store(s)"]
