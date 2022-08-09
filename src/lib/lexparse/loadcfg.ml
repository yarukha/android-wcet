open Lexing
open Cfg_analysis


let print_position outx lb =
  let pos = lb.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_errors lexbuf =
  try Cfgparser.icfg Cfglexer.token lexbuf with 
  |Cfglexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  |Instructions.UnknownInstruction msg -> 
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  |Cfgparser.Error ->
    Printf.fprintf stderr "%a: syntax error in bytecode\n" print_position lexbuf;
    exit (-1)

let cfg lb = 
  match parse_with_errors lb with 
  |Some(p)->p 
  |None-> []
  