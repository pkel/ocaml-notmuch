open Core
open Notmuch

let syncer ~srch_str db store =
  let module Store = TagStore.Make(struct
    let location = store
  end) in
  let f msg =
    let id = Message.get_id msg in
    let tags = Message.get_tags msg in
    Store.update id tags
  in
  let open Query in
  from_string srch_str |> Messages.iter db ~f

let with_db_and_store f =
  let open Printf in
  let open Ext.Result in
  fun () -> Config.load ()
  |> and_then_pair ~fpair:(
    ( fun cfg ->
      Config.get ~section:"database" ~key:"path" cfg
      |> and_then_opt ~err:"Failed to open database" ~f:Database.open_ )
    , fun cfg ->
      Config.get ~section:"ocaml" ~key:"sync_store" cfg )
  |> function
    | Error e -> eprintf "%s\n" e
    | Ok (db, store) -> f db store

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Sync tags to git repository"
    [%map_open
      let srch_lst = anon (sequence ("search-term" %: string))
      in
      let srch_str =
        match srch_lst with
        | [] -> "*"
        | l  -> String.concat ~sep:" " l
      in
      syncer  ~srch_str |> with_db_and_store
    ] |> Command.run
