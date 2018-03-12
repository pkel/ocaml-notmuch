open Core
open Notmuch

let count ~srch_str ~lastmod db =
  let count =
    let open Query in
    from_string srch_str |> Messages.count db
  in
  if lastmod then
    let open Database in
    let rev = get_revision db in
    let uuid = uuid_of_revision rev in
    let nr = nr_of_revision rev in
    printf "%d\t%s\t%s\n" count uuid nr
  else
    printf "%d\n" count

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
    ~summary:"Count messages in notmuch database"
    [%map_open
      let lastmod = flag "lastmod" no_arg ~doc:"Append database revision"
      and srch_lst = anon (sequence ("search-term" %: string))
      in
      let srch_str =
        match srch_lst with
        | [] -> "*"
        | l  -> String.concat ~sep:" " l
      in
      count ~srch_str ~lastmod |> with_db
    ] |> Command.run
