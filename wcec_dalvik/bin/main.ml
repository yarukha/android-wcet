open Wcec_dalvik

let pp = false

let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  let p = Loaddvk.prog lb in
  Printf.printf "Parsing done\n";
  if pp then 
  let out_file = "dvk.cdvk" in 
  let out = open_out out_file in 
  close_in c;
  Pp_dvk.pp_program out p
  else ();
  let _ = Hashdvk.hash_program p in 
  Printf.printf "Hashing done\n"