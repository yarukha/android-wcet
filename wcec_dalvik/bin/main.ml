open Wcec_dalvik

let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  let p = Loaddvk.prog lb in 
  close_in c;
  Pp_dvk.pp_program stdout p;