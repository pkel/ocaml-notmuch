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

let test_all_tags db =
  let module S = Set.Make(String) in
  let should = Database.get_all_tags db |> S.of_list in
  let init = S.empty in
  let f acc el =
    Message.get_tags el |> S.of_list |> S.union acc
  in
  let manual =
    let open Query in
    from_string "*" |> Messages.fold db ~init ~f
  in
  let d = S.diff manual should in
  match S.cardinal d with
  | 0 -> ()
  | _ -> failwith "test_all_tags"

let test_count db =
  let q = "*" in
  let should =
    let open Query in
    from_string q |> Messages.count db
  in
  let manual=
    let init = 0 in
    let f acc el = acc + 1 in
    let open Query in
    from_string "*" |> Messages.fold db ~init ~f
  in
  match should - manual with
  | 0 -> ()
  | _ -> failwith "test_count"

let print_all_tags db =
  Database.get_all_tags db
  |> String.concat " "
  |> Printf.printf "Available tags: %s\n"

let count_all_messages db =
  let open Query in
  from_string "*" |> Messages.count db

let act_on db =
  print_endline "Database opened" ;
  print_revision db ;
  print_all_tags db ;
  count_all_messages db |> Printf.printf "Message count: %d\n" ;
  test_count db ;
  test_all_tags db ;
  Database.close db

let () =
  match Database.open_ "/home/patrik/mail/" with
  | None -> print_endline "failed to open database"
  | Some db -> act_on db

