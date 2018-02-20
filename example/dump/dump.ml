open Core
open Notmuch

let get_db () =
  let loc = "/home/patrik/mail/" in
  match Database.open_ loc with
  | None -> failwith "Could not open database"
  | Some db -> db

let dump str =
  let db = get_db () in
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
  from_string str |> Messages.iter ~f db

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"WARNING: There may be encoding issues compared to notmuch dump."
    [%map_open
      let srch_lst = anon (sequence ("search-term" %: string)) in
      fun () ->
        let srch_str =
          match srch_lst with
          | [] -> "*"
          | l  -> String.concat ~sep:" " l
        in
        dump srch_str
    ] |> Command.run
