open Core
open Notmuch
open Unsafe

let dump ~srch_str db =
  let f msg =
    let open Message in
    let tags = get_tags msg
      |> List.map ~f:(fun tag -> "+" ^ tag)
      |> String.concat ~sep:" "
    in
    let id   = get_id msg in
    Printf.printf "%s -- id:%s\n" tags id
  in
  let () = print_endline "#notmuch-dump batch-tag:3 tags" in
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
    ~summary:"WARNING: There may be encoding issues compared to notmuch dump."
    [%map_open
      let srch_lst = anon (sequence ("search-term" %: string)) in
      let srch_str =
        match srch_lst with
        | [] -> "*"
        | l  -> String.concat ~sep:" " l
      in
      dump ~srch_str |> with_db
    ] |> Command.run
