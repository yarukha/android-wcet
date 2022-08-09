open Lexparse
open Cfg_analysis
open Format

let usage_msg = "wcec [-verbose] -a <app.apk> [-p <powermodel>]"
let verbose = ref false
let input_app = ref ""
let input_pm = ref ""
let speclist =
  [
    ("--verbose", Arg.Set verbose, "Output debug information");
    ("-a", Arg.Set_string input_app, "Set .apk file to analyse");
    ("-p", Arg.Set_string input_pm, "Set power model file to use")
  ]

let anon_fun = 
  Format.printf "unknown argument: %s\n"




let () = 
  Arg.parse speclist anon_fun usage_msg;
  let cfg = 
    match !input_app with 
    |""->failwith "no .apk argument was given"
    |s when not (Filename.check_suffix s ".apk")->failwith (sprintf "the argument %s is not an apk" s)
    |file-> begin
      let f = Zip.open_in file in 
      let l  = Zip.entries f 
      |>List.filter (fun e -> Filename.check_suffix Zip.(e.filename) ".dex") in
      List.iter (fun e -> Zip.copy_entry_to_file f e e.filename ) l; 
      match Sys.command "dexdump -gdo g.dot *.dex" with 
      |0->let c = open_in "g.dot" in
        let lb = Lexing.from_channel c in 
        let cfg = Loadcfg.cfg lb in 
        let _ = Sys.command "rm *.dex g.dot" in 
        close_in c;
        cfg
      |_->failwith "couldnt execute dexdump"
    end in
  let p_m = 
    match !input_pm with 
    |""-> Power_model.empty_m,Power_model.empty_m
    |file->let c = open_in file in 
      let lb = Lexing.from_channel c in 
      let model = Loadmodel.model lb in 
      close_in c; model
  in
  let def_meths = Icfg.Block_Icfg.defined_methods cfg in 
  let icfg = Icfg.Block_Icfg.create cfg def_meths in 
  let module M_c = struct 
    let mt = fst @@ p_m
    let me = snd @@ p_m
  end in 
  let module ILP_construct = Construct_ilp.Build(M_c) in

  let b0 = Block_id.from_meth_string "int android.support.v7.internal.widget.ListPopupWindow.buildDropDown()" in
  let b1 = Block_id.from_meth_string "void android.support.v4.app.ActionBarDrawerToggle$SlideDrawable.draw(android.graphics.Canvas)" in 
  let _ = b0 and _= b1 in 
  ILP_construct.analyze_icfg ~out:(Some"output") icfg def_meths