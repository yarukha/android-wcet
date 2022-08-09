open Lexing


let print_position outx lb =
  let pos = lb.lex_curr_p in
  Printf.fprintf outx "%d:%d" 
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)    

let parse_with_errors lexbuf =
  try Modelparser.model Modellex.token lexbuf with 
  |Modellex.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s in model\n" print_position lexbuf msg;
    None
  |Modelparser.Error ->
    Printf.fprintf stderr "%a: syntax error in model\n" print_position lexbuf;
    exit (-1)

let model lb = 
  match parse_with_errors lb with 
  |Some(p)->p 
  |None-> failwith "error in model file"
  