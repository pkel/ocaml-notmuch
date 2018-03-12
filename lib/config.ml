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

let parse_with_error lexbuf =
  let open Lexer in
  let open Result in
  try Parser.cfg Lexer.read lexbuf |> ok with
  | SyntaxError msg ->
    Printf.sprintf "%s: %s" (get_position lexbuf) msg |> err
  | Parser.Error ->
    Printf.sprintf "%s: syntax error" (get_position lexbuf) |> err

let from_file filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
  let ret = parse_with_error lexbuf in
  close_in inx ;
  ret
