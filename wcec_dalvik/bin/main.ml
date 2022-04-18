open Wcec_dalvik

let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  let prog = Loaddvk.prog lb in 
  close_in c;
  Dvk.pp_program prog stdout;