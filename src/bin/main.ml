open Lexparsecfg
open Cfg_analysis
open Solver


let usage_msg = "wcec [-verbose] <file>"
let verbose = ref false
let input_files = ref []
let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
  ]

let anon_fun filename =
  input_files := filename::!input_files




let () = 
  Arg.parse speclist anon_fun usage_msg;
  let l = 
    List.fold_left ( fun l' file ->
      let c = open_in file in 
      Printf.printf "\nInput file: %s\n" file;
      let lb = Lexing.from_channel c in 
      let l'' = Loadcfg.cfg lb in 
      Cfg.merge l'' l'
      ) []
      (List.rev !input_files) in
    Printf.printf "methods number: %i\n" (List.length l);
    Printf.printf "not exception edges number: %i\n" 
    (List.fold_left (fun i (_,dg) ->i + Cfg.(List.length dg.taken_edges + List.length dg.regular_edges)  ) 0 l);
    (* let cfg = Link_methods.consider_invokes (Simplifycfg.simplify l) in  *)
    let icfg = Cfg2icfg.transform_cfg l in
    Printf.printf "\ncfg simplification done (at least the easy part)\n";
    (* Dbg_tools.defined_methods icfg; *)
    (* Dbg_tools.undefined_methods icfg *)
    (*Test_solver.big_time icfg;*)
    Test_lp.f icfg
