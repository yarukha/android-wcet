open Instructions
open Icfg
open Printf

(**return a list of the invoked methods in a cfg*)
let invoked_methods (cfg : block cfg) = 
  Hashtbl.fold (
    fun _ n l -> (List.filter_map (fun i ->
      match i.op with |Opn(Invoke(_),_)-> Some (List.hd i.args) |_->None) n.value)@l
  ) cfg [] 

(**return the list of all the invoked methods in an icfg*)
let all_invokes_methods (icfg: block icfg) =
  Hashtbl.fold (
    fun _ cfg l' -> 
      let invoked_m = invoked_methods cfg in 
      List.fold_left (fun l'' m -> if List.mem m l'' then l'' else m::l'') l' invoked_m
  ) icfg []

(**prints all the undefined methods, ie the Android methods*)
let undefined_methods icfg = 
  let l = all_invokes_methods icfg in 
  List.iter (
    fun m -> match Hashtbl.find_opt icfg (Cfg.M_id(m)) with 
      |None->printf "%s\n" m |Some _ -> ()
  ) l