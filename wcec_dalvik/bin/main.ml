open Analysis
open Lexparse





let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  Printf.printf "Input file: %s\n" file;
  let lb = Lexing.from_channel c in 
  let p = Loadprog.prog lb in
  Printf.printf "Lexing and Parsing done\n";
  let hp = Hashdvk.hash_program p in 
  Printf.printf "Hashing done\n";
  let _ = Hash2cfg.methods_number hp in ();