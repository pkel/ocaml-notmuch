{
open Lexing
open Config_parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let newline = white* ('\r' | '\n' | "\r\n")
let key = ['a'-'z' '_']+

rule read =
  parse
  | newline { next_line lexbuf ; read lexbuf }
  | white  { read lexbuf }
  | '#' { read_comment lexbuf }
  | '=' { read_value (Buffer.create 10) lexbuf }
  | '[' { LEFT_BRACK }
  | ']' { RIGHT_BRACK }
  | key { KEY (lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
and read_value buf =
  parse
  | newline { next_line lexbuf ; VALUE (Buffer.contents buf) }
  | _ { Buffer.add_string buf (lexeme lexbuf) ; read_value buf lexbuf }
and read_comment =
  parse
  | newline { next_line lexbuf ; read lexbuf }
  | eof { EOF }
  | _ { read_comment lexbuf }

