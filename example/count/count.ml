open Core
open Notmuch

let get_db () =
  let loc = "/home/patrik/mail/" in
  match Database.open_ loc with
  | None -> failwith "Could not open database"
  | Some db -> db

let count str lastmod =
  let db = get_db () in
  let count =
    let open Query in
    from_string str |> Messages.count db
  in
  if lastmod then
    let open Database in
    let rev = get_revision db in
    let uuid = uuid_of_revision rev in
    let nr = nr_of_revision rev in
    printf "%d\t%s\t%s\n" count uuid nr
  else
    printf "%d\n" count

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Count messages in notmuch database"
    [%map_open
      let lastmod = flag "lastmod" no_arg ~doc:"Append database revision"
      and srch_lst = anon (sequence ("search-term" %: string)) in
      fun () ->
        let srch_str =
          match srch_lst with
          | [] -> "*"
          | l  -> String.concat ~sep:" " l
        in
        count srch_str lastmod
    ] |> Command.run
