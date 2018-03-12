open Core
open Notmuch

let act file =
  let itm (key, value) =
    printf "%s=%s\n" key value
  in
  let sec (head, items) =
    printf "[%s]\n" head;
    List.iter ~f:itm items
  in
  match Config.from_file file with
  | Error err -> print_endline err
  | Ok cfg -> List.iter ~f:sec cfg

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Parse and print config"
    [%map_open
      let file = anon ("cfg"%:string) in
      fun () -> act file
    ] |> Command.run
