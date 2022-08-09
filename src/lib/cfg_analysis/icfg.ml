

module Make_Icfg(Ord: Map.OrderedType) = struct 
  module M = Map.Make(Ord)
  type key = M.key 
  module S = Set.Make(Ord)
  module Edge = struct 
    type edge_type = Regular | Taken |Exception |Invoke |After_invoke
    type t = {next : key; edg_t : edge_type}
    let compare e1 e2 = Ord.compare e1.next e2.next
  end
  module Edg_set = Set.Make(Edge)
  type 'a node = {value : 'a; next : Edg_set.t}
  type 'a t = 'a node M.t

  (**the empty icfg*)
  let empty =
    M.empty

  (**add a node to a cfg, without modifying the return map*)
  let add  : key -> 'a node -> 'a t -> 'a t=
    M.add
  
  (**union of two icfg*)
  let union : 'a t -> 'a t-> 'a t = 
    fun g g' -> M.union (fun _ _ _ -> None) g g'
  
  
  (**number of nodes of an icfg*)
  let size : 'a t -> int = 
    M.cardinal 

  (**iterate on all nodes*)  
  let iter_on_nodes :(key -> 'a node -> unit) -> 'a t -> unit = 
    fun f icfg -> M.iter f icfg

    
  (**find node value*)
  let find_value : key -> 'a t -> 'a= 
    fun k g -> (M.find k g).value

  (**find successor of node*)
  let find_next = 
    fun k g -> (M.find k g).next

  (**set of keys matching a property*)
  let filter_set : (key->bool) -> 'a t -> S.t= 
    fun f g -> M.fold (fun k _ s -> if f k then S.add k s else s) g (S.empty)
end


module Block_Icfg = struct 
  module Icfg = Make_Icfg(Block_id)
  module S_Meth = Icfg.S
  module S_Edg = Icfg.Edg_set
  type key = Icfg.key 
  type t = Instructions.block Icfg.t  
  
  (**construct a graph from a block, taking into account invoke splitting: each invoke instruction has its own block
      No edge is added for native methods
      we also return in the couple the key of the last split block, usefull for the return set*)
  let split_block b_id next b def_meths= 
    let g0 = Icfg.empty in 
    let open Instructions in 
    let build_next_regular count = Icfg.Edg_set.singleton {next = {b_id with sub_id = count};edg_t = Regular} in  
    let build_invoke b_id = Icfg.Edg_set.singleton {next = b_id;edg_t=Invoke} in
    let build_after_invoke count = Icfg.Edg_set.singleton {next ={b_id with sub_id=count};edg_t = After_invoke} in 
    let build_return = Icfg.Edg_set.singleton {next=Block_id.return_node b_id; edg_t=Regular} in
    let is_invoke = function |[x]->(match x.op with Opn(Invoke(_),_)->true |_->false )|_->false in
    let add_count count = Icfg.add {b_id with sub_id =  count} in
    let next' = if Icfg.Edg_set.is_empty next then build_return else next in 
    let rec foo instr_l last_block g count = begin
      match instr_l,last_block with 
      |[],[]->g
      |[],last_block->
        if is_invoke last_block then (
          let invoke_id = Block_id.from_meth_string (List.hd (List.hd last_block).args) in 
          if S_Meth.mem invoke_id def_meths then 
            let g_ = add_count count {value = last_block; next = Icfg.Edg_set.union (build_invoke invoke_id) (build_after_invoke (count +1))} g in 
            add_count (count+1) {value = [];next = next'} g_ 
          else
            add_count count {value = last_block;next = next'} g)
        else
          add_count count {value = List.rev last_block;next =next'} g
      |i::q,[]-> foo q [i] g count
      |i::q,last_block->
        if is_invoke last_block then (
          let invoke_id = Block_id.from_meth_string (List.hd (List.hd last_block).args) in 
          if S_Meth.mem invoke_id def_meths then 
            let g' = add_count count {value = last_block; next = Icfg.Edg_set.union (build_invoke invoke_id) (build_after_invoke (count +1))} g in 
            let g'' = add_count (count +1) {value = [];next = build_next_regular (count +2)} g' in
            foo (i::q) [] g'' (count +2)
          else
            let g' = add_count count {value = last_block; next = build_next_regular (count +1)} g in 
            foo (i::q) [] g' (count +1)
            )
        else
          if is_invoke [i] then 
            let g' = add_count count {value = List.rev last_block; next = build_next_regular (count +1)} g in
             foo q [i] g' (count +1) 
          else
            foo q (i::last_block) g (count)
          end in 
    foo b [] g0 0


  
  (**create a cfg from a method*)
  let add_method b_id dg def_meths= 
    let open Cfg in
    let open Icfg in
    let b_id_from_pos p :key = let N_id(n_id) = p.node in {b_id with n_id =n_id} in
    let g0 = Icfg.empty in
    let h = Hashtbl.create 10 in  
    let runthrough edges edg_t= 
      List.iter (
        fun (p1,p2)->let former_next=   Hashtbl.find_opt h (b_id_from_pos p1) in match former_next with 
          |None->(* Block_id.pp (b_id_from_pos p1); *) Hashtbl.add h (b_id_from_pos p1) (Edg_set.singleton {next = b_id_from_pos p2;edg_t})
          |Some(edg_s)->Hashtbl.replace h (b_id_from_pos p1) (Edg_set.add {next=b_id_from_pos p2;edg_t} edg_s)
      ) edges
    in runthrough dg.regular_edges Regular; runthrough dg.exception_edges Exception; runthrough dg.taken_edges Taken; 
    let g = 
      (*we add each block of the method to the cfg and we keep track of the return nodes to add them *)
    List.fold_left (
      fun g (N_id(n_id),b) ->
         let b_id' = {b_id with n_id = n_id} in 
         let next = match  Hashtbl.find_opt h b_id' with |None -> Edg_set.empty |Some(s) -> s in
         let g' = split_block b_id' next b def_meths in 
         let g'' = union g g' in 
         g''

    )  g0 dg.nodes in Hashtbl.clear h;
    Icfg.add (Block_id.return_node b_id) {value = [];next = Icfg.Edg_set.empty} g 

  
  (**create an icfg from a cfg*)
  let create cfg def_meths = 
    let open Cfg in 
    let open Icfg in 
    let g0 = empty in 
    List.fold_left (
      fun g (M_id(m_id),dg) -> let b_id:key= {m_id;n_id="0";sub_id=0} in 
        let g' = add_method b_id dg def_meths in 
        let g'' = union g g' in 
        g''
    ) g0 cfg
  
  (**set of all the methods entry point*)
  let defined_methods (cfg: 'a Cfg.cfg) = 
    let s0 = S_Meth.empty in 
    List.fold_left (
      fun s (Cfg.M_id(m_id),_) -> S_Meth.add (Block_id.from_meth_string m_id) s
    ) s0 cfg


  (**for an invoke node, split the  of invoke and after_invoke nodes *)
  let split_invoke : key -> t -> (key*key) option=
  fun k g -> 
    let next = Icfg.find_next k g in 
    let b = Icfg.Edg_set.exists (fun n -> n.edg_t=Invoke) next in 
    if not b then None 
    else
      Icfg.Edg_set.fold (
        fun n opt -> match n.edg_t,opt with 
        |Invoke,None|After_invoke,None->Some(n.next,n.next)
        |_,None->None
        |Invoke,Some(_,aft)->Some(n.next,aft)|After_invoke,Some(inv,_)->Some(inv,n.next)
        |_,Some(x)->Some(x)
      ) next None

  let pp_node : key -> t -> unit =
    fun k g -> let n = Icfg.find_next k g and b = Icfg.find_value k g in 
    Format.printf "\tvalue size: %i\n\tnext:\n" (List.length b);
    Icfg.Edg_set.iter (
      fun edg -> Format.printf "\t\t%s -> %s\n" 
      (match edg.edg_t with |Regular->"Regular"|Taken->"Taken"|Exception->"Exception"|Invoke->"Invoke"|After_invoke->"After_Invoke")
      (Block_id.to_string edg.next);

    ) n
 
  let pp_method : key -> t -> unit = 
    fun k icfg -> match Block_id.is_method_entry k with |false->failwith "tried to print method with wrong entry point"
    |_->
      let m = Icfg.M.filter (fun k' _ -> k'.m_id = k.m_id) icfg in 
      let open Format in 
      printf "Method with entry point:\n"; Block_id.pp k;
      printf "Containing nodes:\n"; Icfg.M.iter (fun k' _ -> Block_id.pp k'; pp_node k' icfg) m


  let method_set = 
    fun k g -> match Block_id.is_method_entry k with 
    |false->failwith (Format.sprintf "%s is not a method entry" (Block_id.to_string k))
    |true->Icfg.filter_set (Block_id.same_method k) g

end


