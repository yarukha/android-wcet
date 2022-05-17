open Analysis
open Lexparse


let usage_msg = "wcec [-verbose] <file>"
let verbose = ref false
let input_files = ref []
let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
  ]

let anon_fun filename =
  input_files := filename::!input_files


(*let id ic oc =
  let i = Xmlm.make_input (`Channel ic) in
  let o = Xmlm.make_output (`Channel oc) in
  let rec pull i o depth =
    Xmlm.output o (Xmlm.peek i);
    match Xmlm.input i with
    | `El_start _ -> pull i o (depth + 1)
    | `El_end -> if depth = 1 then () else pull i o (depth - 1)
    | `Data _ -> pull i o depth
    | `Dtd _ -> assert false
  in
  Xmlm.output o (Xmlm.input i); (* `Dtd *)
  pull i o 0;
  if not (Xmlm.eoi i) then invalid_arg "document not well-formed"*)

let () = 
  Arg.parse speclist anon_fun usage_msg;
  List.iter (fun file -> 
    let c = open_in file in 
    Printf.printf "\nInput file: %s\n" file;
    let lb = Lexing.from_channel c in 
    let p = Loadprog.prog lb in
    Printf.printf "Lexing and Parsing done\n";
    let hp = Hashdvk.hash_program p in 
    Printf.printf "Hashing done\n";
    let h_m_number = Hash2cfg.methods_number hp in
    if !verbose then 
    Printf.printf "Hashed methods number: %i\n" h_m_number;
    let cfg = Hash2cfg.transform_program hp in 
    Printf.printf "CFG building done\n";
    if !verbose then begin
      let cfg_method_n =  Dbg_tools.cfg_method_number cfg in 
      Printf.printf "CFG methods number: %i\n" cfg_method_n;
      let mult_r = Dbg_tools.multiple_returns_count cfg in 
      Printf.printf "Multiple returns number: %i\n" mult_r
    end;
    print_newline ();)  (List.rev !input_files)