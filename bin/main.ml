(* open Analysis *)
open Lexparsecfg
open Cfg_analysis


let usage_msg = "wcec [-verbose] <file>"
let verbose = ref false
let input_files = ref []
let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
  ]

let anon_fun filename =
  input_files := filename::!input_files



(*
let () = 
  Arg.parse speclist anon_fun usage_msg;
    List.iter (fun file -> 
    let c = open_in file in 
    Printf.printf "\nInput file: %s\n" file;
    let lb = Lexing.from_channel c in 
    let p = Loadprog.prog lb in
    Printf.printf "Lexing and Parsing done\n";
    let cfg = Dvk2cfg.transform_program p in 
    Printf.printf "CFG building done\n";
    let _ = Simplify_cfg.simplify_icfg cfg in 
    Printf.printf "CFG simplification done\n";
    if !verbose then begin
      let cfg_method_n =  Dbg_tools.cfg_method_number cfg in 
      Printf.printf "CFG methods number: %i\n" cfg_method_n;
      let mult_r = Dbg_tools.multiple_returns_count cfg in 
      Printf.printf "Multiple returns number: %i\n" mult_r
    end;
    print_newline ();)  (List.rev !input_files)*)

let () = 
  Arg.parse speclist anon_fun usage_msg;
  let _ = 
    List.iter( fun file ->
      let c = open_in file in 
      Printf.printf "\nInput file: %s\n" file;
      let lb = Lexing.from_channel c in 
      let l = Loadcfg.cfg lb in 
      Printf.printf "Lexparse done \n";
      Printf.printf "methods number: %i\n" (List.length l);
      Printf.printf "not exception edges number: %i\n" 
      (List.fold_left (fun i (_,dg) ->i + Cfg.(List.length dg.taken_edges + List.length dg.regular_edges)  ) 0 l);
      let cfg = Simplifycfg.simplify l in 
      Printf.printf "\ncfg simplification done (at least the easy part)\n";
      Printf.printf "number of nodes: %i\n" (Hashtbl.length cfg) ;
      Printf.printf "edges number: %i\n" (Hashtbl.fold (fun _ n i -> (i + List.length Scfg.(n.next)) ) cfg 0 );
      Dbg_tools.found_invokes ~found:false cfg;
      )
      (List.rev !input_files) 
    in ()