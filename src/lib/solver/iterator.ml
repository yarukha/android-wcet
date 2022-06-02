open Cfg_analysis
open Icfg
open Lattices
open Abstractions

module type Cfg = sig 
  type t 
  val cfg : t cfg
end

module type Iterator = sig 
  type abstract 
  type concrete

  val join_list: abstract list -> abstract

  (**create an abstract cfg, structurally equal to the concrete one, initialised with bottom*)
  val init : concrete cfg -> abstract cfg

  (**iterate on the abstract cfg and returns it, this modifies the input abstract cfg*)
  val iterate : concrete cfg -> (abstract cfg -> abstract cfg)

  (**return the list of the abstract values of final nodes*)
  val final_abstract_values : abstract cfg -> abstract list

  val final_value : abstract cfg -> abstract 
end



module MakeIterator (A:SemiLattice) (F:Abstraction with type abstract =A.t) : 
  (Iterator with type abstract = F.abstract and type concrete = F.concrete) = struct
  type abstract = F.abstract
  type concrete = F.concrete 

  let join_list l = 
    match l with 
    |[]-> A.bottom
    |x::q-> List.fold_left A.join x q
  let init cfg = 
    let n = Hashtbl.length cfg in 
    let h = Hashtbl.create n in 
    Hashtbl.iter (
      fun n_id x -> Hashtbl.add h n_id {value = A.bottom;next = x.next}
    ) cfg;
    h
  let iterate ccfg acfg = 
    (* Printf.printf "number of nodes: %i\n" (Icfg.node_numbers ccfg); *)
    let rec foo stack = 
      match stack with 
      |[]->()
      |x::q->
        let pred_nodes = predecessors acfg x in 
        let pred_values = List.map (fun y -> (find_node acfg y).value) pred_nodes in 
        let pred_a = join_list pred_values in 
        let c = (find_node ccfg x).value in 
        let new_a = F.interpretation c pred_a in 
        let a = (find_node acfg x).value in 
        if not (A.equal a new_a) then begin
          Hashtbl.replace acfg x {value = new_a;next = (Hashtbl.find ccfg x).next};
          let succ = Icfg.successors acfg x in 
          let new_stack = succ@q in
          foo new_stack
          end
        else
          foo q;
      in
    foo [Cfg.N_id("0")];
    acfg

  let final_abstract_values acfg = 
    let l = (no_successors acfg) in 
    List.map (
      fun n_id -> (find_node acfg n_id).value
    ) l
  let final_value acfg = 
    let l = final_abstract_values acfg in 
    join_list l
end


