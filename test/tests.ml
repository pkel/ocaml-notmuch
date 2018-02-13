open Notmuch

exception Failed_test of string

let option_test str = function
  | None -> raise (Failed_test str)
  | Some a -> a

(* Check behaviour of Database.rev_compare *)
let () =
  let m = "rev_compare" in
  let open Database in
  let l = [ "a | 1", "b | 1", Incompatible
          ; "b | 1", "b | 2", Newer
          ; "b | 1", "b | 1", Same
          ; "b | 2", "b | 1", Older
          ] in
  let check (a,b,r) =
    let a = revision_of_string a |> option_test m in
    let b = revision_of_string b |> option_test m in
    if rev_compare a b = r then () else raise (Failed_test m)
  in
  List.iter check l


let print_revision db =
  let open Database in
  get_revision db |> string_of_revision |> Printf.printf "Revision: %s\n"

let act_on db =
  print_endline "Database opened" ;
  print_revision db ;
  Database.close db

let () =
  match Database.open_ "/home/patrik/mail/" with
  | None -> print_endline "failed to open database"
  | Some db -> act_on db

