open Analysis
open Lexparse

let cfg_method_number p = 
  match p with 
  |Cfg.Empty_cfg -> 0
  |Cfg.Icfg(cfg)->Hashtbl.length cfg.cfgs

let multiple_returns_count p = 
  match p with 
  |Cfg.Empty_cfg -> 0
  |Cfg.Icfg(cfg)->
    let i = ref 0 in 
    Hashtbl.iter (
      fun _ m -> match m with |Cfg.Empty_method -> ()
      |Method(m')-> if List.length (m'.exit) > 0  then incr(i);
    ) cfg.cfgs;
    !i


let cfg_info = false

let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  Printf.printf "Input file: %s\n" file;
  let lb = Lexing.from_channel c in 
  let p = Loadprog.prog lb in
  Printf.printf "Lexing and Parsing done\n";
  let hp = Hashdvk.hash_program p in 
  Printf.printf "Hashing done\n";
  let h_m_number = Hash2cfg.methods_number hp in
  Printf.printf "Hashed methods number: %i\n" h_m_number;
  let cfg = Hash2cfg.transform_program hp in 
  Printf.printf "CFG building done\n";
  if cfg_info then begin
    let cfg_method_n = cfg_method_number cfg in 
    Printf.printf "CFG methods number: %i\n" cfg_method_n;
    let mult_r = multiple_returns_count cfg in 
    Printf.printf "Multiple returns number: %i\n" mult_r
  end;