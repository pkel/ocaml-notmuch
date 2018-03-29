open Notmuch
open Lwt.Infix

module StoreConfig( C : sig val location : string end ) = struct
  let author =
    let open TagStore in
    let user = Unix.getlogin () in
    let host = Unix.gethostname () in
    { name = "onm sync"
    ; mail = user ^ "@" ^ host
    }
  let location = C.location
end

let fail_opt ?(code=1) ~err = function
  | Some v -> v
  | None -> (Printf.eprintf "%s\n" err ; exit code)

let fail ?(code=1) = function
  | Ok v -> v
  | Error e -> (Printf.eprintf "%s\n" e ; exit code)

module ConfigFail = struct
  let load () = Config.load ()   |> fail ~code:121
  let get ~section ~key cfg =
    Config.get ~section ~key cfg |> fail ~code:122
end

let write srch_lst =
  let module C = ConfigFail in
  let cfg = C.load () in
  let db = C.get ~section:"database" ~key:"path" cfg
    |> Database.open_
    |> fail_opt ~err:"Failed to open database" ~code:123
  in
  let local  = C.get ~section:"ocaml" ~key:"sync_store_local"  cfg in
  let module Store =
    TagStore.Make(StoreConfig(struct let location = local end))
    in
  let lwt =
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
  in
  Lwt_main.run lwt

let read = write

let pull () =
  let module C = ConfigFail in
  let cfg = C.load () in
  let local  = C.get ~section:"ocaml" ~key:"sync_store_local"  cfg in
  let remote = C.get ~section:"ocaml" ~key:"sync_store_remote" cfg in
  let module Store =
    TagStore.Make(StoreConfig(struct let location = local end))
    in
  Store.pull remote |> Lwt_main.run |> fail ~code:5

let push () =
  let module C = ConfigFail in
  let cfg = C.load () in
  let local  = C.get ~section:"ocaml" ~key:"sync_store_local"  cfg in
  let remote = C.get ~section:"ocaml" ~key:"sync_store_remote" cfg in
  let module Store =
    TagStore.Make(StoreConfig(struct let location = local end))
    in
  Store.push remote |> Lwt_main.run |> fail ~code:5

open Cmdliner
open Term

let push_cmd =
  let doc = "Push local changes to remote" in
  const push $ const (), info ~doc "push"

let pull_cmd =
  let doc = "Pull remote changes to local" in
  const pull $ const (), info ~doc "pull"

let srch_lst =
  let doc = "Overwrite the default search term" in
  let env = Arg.env_var "NOTMUCH_SEARCH_TERM" ~doc in
  let doc = "Notmuch search term to filter operation on" in
  Arg.(value
    & pos_all string ["path:**"]
    & info [] ~env ~docv:"SEARCH_TERM" ~doc )

let write_cmd =
  let doc = "Write changes to local store" in
  const write $ srch_lst, info ~doc "write"

let read_cmd =
  let doc = "Read changes from local store" in
  const read $ srch_lst, info ~doc "read"

