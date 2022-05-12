open Analysis
open Lexparse
open Lexing


let print_position outx lb =
  let pos = lb.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_errors lexbuf =
  try Dvkparser.program Dvklexer.token lexbuf with 
  |Dvklexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  |Dvk.UnknownInstruction msg-> 
    Printf.fprintf stderr "%a:\nUnknown instruction: %s\n" print_position lexbuf msg;
    None
  |Dvk.NotTranslated msg-> 
    Printf.fprintf stderr "%a:\nNot translated: %s\n" print_position lexbuf msg;
    None
  |Dvkparser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let prog lb = 
  match parse_with_errors lb with 
  |Some(p)->p 
  |None-> 
    failwith "empty file"
  

let pp = false
let entry = "Landroid/support/v4/accessibilityservice/AccessibilityServiceInfoCompat$AccessibilityServiceInfoVersionImpl.getCanRetrieveWindowContent"

let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  Printf.printf "Input file: %s\n" file;
  Printf.printf "Lexing done\n";
  let p = prog lb in
  Printf.printf "Parsing done\n";
  if pp then 
  let out_file = "dvk.cdvk" in 
  let out = open_out out_file in 
  close_in c;
  Pp_dvk.pp_program out p
  else ();
  let hp = Hashdvk.hash_program p in 
  Printf.printf "Hashing done\n";
  let _ = Hash2cfg.transform_program hp (Cfg.Descriptor(entry)) in ()