open Analysis
open Lexparse



let pp = false
let entry = "Landroid/support/v4/accessibilityservice/AccessibilityServiceInfoCompat$AccessibilityServiceInfoVersionImpl.getCanRetrieveWindowContent"
let entry_method = Cfg.method_id_of_string entry

let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  Printf.printf "Input file: %s\n" file;
  Printf.printf "Lexing done\n";
  let p = Loadprog.prog lb in
  Printf.printf "Parsing done\n";
  if pp then 
  let out_file = "dvk.cdvk" in 
  let out = open_out out_file in 
  close_in c;
  Pp_dvk.pp_program out p
  else ();
  let hp = Hashdvk.hash_program p in 
  Printf.printf "Hashing done\n";
  let _ = Hash2cfg.transform_program hp (entry_method) in ()