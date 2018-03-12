open Lexing
module Lexer = Config_lexer
module Parser = Config_parser

type item =
  string * string

type section =
  string * item list

type t = Config_type.t

let get_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_bol + 1)

let err a = Error a
let ok a = Ok a

let parse_with_error lexbuf =
  let open Printf in
  let open Lexer in
  try Parser.cfg Lexer.read lexbuf |> ok with
  | SyntaxError msg ->
    sprintf "%s: %s" (get_position lexbuf) msg |> err
  | Parser.Error ->
    sprintf "%s: invalid syntax" (get_position lexbuf) |> err

let from_file filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  let basename = Filename.basename filename in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = basename } ;
  let ret = parse_with_error lexbuf in
  close_in inx ;
  ret

let locate () =
  let candidates = [
    Sys.getenv_opt "NOTMUCH_CONFIG" ;
    Sys.getenv_opt "HOME" |> Option.map ~f:(fun x -> x ^ "/.notmuch-config") ;
    Some ".notmuch-config" ]
  in
  let filter = function
    | None -> false
    | Some fp -> Sys.file_exists fp
  in
  match List.find_opt filter candidates with
  | Some (Some fp) -> Ok fp
  | _ -> Error "Could not locate configuration file"

let load ?custom () =
  match custom with
  | Some fp -> from_file fp
  | None ->
      match locate () with
      | Ok fp -> from_file fp
      | Error err -> Error err

let get ~section ~key cfg =
  let open Printf in
  let open Result in
  List.assoc_opt section cfg
      |> of_option ~err:(sprintf
      "Could not find section %s in config" section)
      |> and_then_opt ~f:(fun lst ->
        List.assoc_opt key lst) ~err:(sprintf
        "Could not find key %s in section %s of config" key section)
