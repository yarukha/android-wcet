open Instructions
open Icfg
open Printf


module S = Set.Make(String)




(**return a set of the invoked methods in a cfg*)
let invoked_methods (cfg : block cfg) = 
  Hashtbl.fold (
    fun _ n s -> 
      List.fold_left (fun s' i -> 
        match i.op with 
        |Opn(Invoke(inv),_) -> S.add ((List.hd i.args)^"%"^(Instr_tools.invoke_type_to_string inv)) s' 
        |_ -> s'  ) s n.value)
   cfg S.empty

(**return the set of all the invoked methods in an icfg*)
let all_invokes_methods (icfg: block icfg) =
  Hashtbl.fold (
    fun _ cfg s -> S.union s (invoked_methods cfg)
  ) icfg S.empty

(**prints all the undefined methods, ie the Android methods*)
let undefined_methods icfg = 
  printf "\nNot defined methods:\n";
  let s = all_invokes_methods icfg in 
  S.iter (
    fun m_inv ->let m = List.hd (String.split_on_char '%' m_inv) in  match Hashtbl.find_opt icfg (Cfg.M_id(m)) with 
      |None->printf "%s\n" m_inv |Some _ -> ()
  ) s

let defined_methods icfg = 
  printf "\nWell defined methods:\n";
  let s = all_invokes_methods icfg in 
  S.iter (
    fun m_inv ->let m = List.hd (String.split_on_char '%' m_inv) in  match Hashtbl.find_opt icfg (Cfg.M_id(m)) with 
      |Some _->printf "%s\n" m_inv |None -> ()
  ) s