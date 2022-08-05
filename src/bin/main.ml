open Lexparsecfg
open Cfg_analysis
open Format

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
      let f = Zip.open_in file in 
      let l  = Zip.entries f 
      |>List.filter (fun e -> Filename.check_suffix Zip.(e.filename) ".dex") in
      List.iter (fun e -> Zip.copy_entry_to_file f e e.filename ) l; 
      match Sys.command "dexdump -gdo g.dot *.dex" with 
      |0->let c = open_in "g.dot" in 
        Format.printf "eheh";
        let lb = Lexing.from_channel c in 
        let l'' = Loadcfg.cfg lb in 
        let _ = Sys.command "rm *.dex g.dot" in 
        Cfg.merge l'' l'
      |_->failwith "couldnt execute dexdump"
      ) []
      (List.rev !input_files) in
    printf "methods number: %i\n" (List.length l);
    printf "not exception edges number: %i\n" 
    (List.fold_left (fun i (_,dg) ->i + Cfg.(List.length dg.taken_edges + List.length dg.regular_edges)  ) 0 l);
  let def_meths = Icfg.Block_Icfg.defined_methods l in 
  let icfg = Icfg.Block_Icfg.create l def_meths in 
  printf "number of nodes in icfg: %i\n" (Icfg.Block_Icfg.Icfg.size icfg);  
  let b0 = Block_id.from_meth_string "int android.support.v7.internal.widget.ListPopupWindow.buildDropDown()" in
  let b1 = Block_id.from_meth_string "void android.support.v4.app.ActionBarDrawerToggle$SlideDrawable.draw(android.graphics.Canvas)" in 
  let _ = b0 and _= b1 in 
  (* Construct_ilp.analyze_icfg ~out:(Some("output/ilp")) icfg def_meths *)
  Construct_ilp.debug_ilp b0 icfg def_meths