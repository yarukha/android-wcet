open Analysis
open Cfgood
open Lattices

module type Cfg_entry = sig
  type t
  val entry : node_position
  val cfg : t list cfg
end

module type Abstraction  = sig
  type concrete
  type abstract
  val interpretation : concrete -> (abstract -> abstract)
end


module type Iterator = sig 
  type concrete
  type abstract 
  val init_abstraction : unit -> abstract cfg
  val interprete_list : concrete list -> (abstract -> abstract)
  val list_join : abstract list -> abstract
  val fixpoint : unit -> abstract cfg
end



module MakeIterator (C: Cfg_entry) (A:SemiLattice) 
(F: Abstraction with type concrete = C.t and type abstract = A.t) :Iterator = struct 
  type concrete = C.t
  type abstract = A.t
  let init_abstraction () = 
    match C.cfg with 
    |Empty_cfg -> Empty_cfg
    |Cfg(h)->
      let n = Hashtbl.length h in 
      let bot_h = Hashtbl.create n in 
      Hashtbl.iter (
        fun node_pos (Node(_,next)) -> 
          Hashtbl.add bot_h node_pos (Node(A.bottom,next))
      ) h; 
      Cfg(bot_h)
  let interprete_list l =
    fun a_init -> List.fold_left (fun a c -> (F.interpretation c) a ) a_init l

  let list_join l = 
    List.fold_left (fun jointure a -> A.join jointure a ) A.bottom l
  let fixpoint () = 
    let concrete_h = match C.cfg with |Empty_cfg -> failwith "tried to get a fixpoint for an empty cfg" |Cfg(h')->h' in
    match (init_abstraction ()) with 
    |Empty_cfg -> Empty_cfg 
    |Cfg(h) -> 
      let get_predecessors node_pos = 
        Hashtbl.fold (
          fun current_pos (Node(_,next)) predecessors_list->
            if List.mem node_pos next then current_pos::predecessors_list else predecessors_list 
        ) h [] in 
      let rec update_stack stack =
        match stack with 
        |[] -> ()
        |x::q->  
          let pred = get_predecessors x in 
          let predecessors_a_value = 
            list_join (List.map (fun p -> let Node(a,_) = Hashtbl.find h p in a) pred) in 
          let Node(c_value,_) = Hashtbl.find concrete_h x in 
          let Node(a_value,next) = Hashtbl.find h x in 
          let new_a_value = (interprete_list c_value) predecessors_a_value in 
          if A.equal new_a_value a_value then 
            update_stack q 
          else
            update_stack (next@q) 
          in 
      update_stack [C.entry];
      Cfg(h)
end
