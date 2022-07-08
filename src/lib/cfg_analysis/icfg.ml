open Format

module Block_id = struct 
  type t = {m_id : string; n_id: string; sub_id : int}
  type t1 = {n_id : string; sub_id:int}
  type t2 = {sub_id:int}
  let compare  = compare 
  let method_string b= b.m_id
  let  node_string b = b.n_id
  let subnode_string b = sprintf "%i" b.sub_id
  let create = 
    let open Cfg in fun (M_id(m_id)) (N_id(n_id)) sub_id -> {m_id;n_id;sub_id}
  let to_string b = sprintf "%s node%s subnode%i" b.m_id b.n_id b.sub_id
  let pp b = printf "%s\n" (to_string b)
end


module Make_Icfg(Ord: Map.OrderedType) = struct 
  module M = Map.Make(Ord)
  type key = M.key 
  module Edge = struct 
    type edge_type = Regular | Taken |Exception |Invoke 
    type t = {next : key; edg_t : edge_type}
    let compare e1 e2 = Ord.compare e1.next e2.next
  end
  module Edg_set = Set.Make(Edge)
  type 'a node = {value : 'a; next : Edg_set.t}
  type 'a t = ('a node) M.t
  let empty :'a t = M.empty 
  let find : key -> 'a t ->'a node = M.find 
  let add : key -> 'a node -> 'a t -> 'a t= M.add
  let merge : 'a t -> 'a t -> 'a t= M.merge (fun _ _ _ -> None) 
end


module Block_Icfg = struct 
  module Icfg = Make_Icfg(Block_id)
  type t = Instructions.block Icfg.t 
  let split_block b_id next b = 
    let g0 = Icfg.empty in 
    let open Instructions in 
    let build_next_regular count = Icfg.Edg_set.singleton {next = {b_id with sub_id = count};edg_t = Regular} in  
    let build_invoke args = Icfg.Edg_set.singleton {next = {m_id=List.hd args;n_id="0";sub_id=0};edg_t=Invoke} in
    let rec foo instr_l last_block g count = 
      match instr_l,last_block with 
      |[],[]->g
      |[],_->Icfg.add {b_id with sub_id=count} {value=List.rev last_block;next} g
      (*the case where an invoke is the last instruction of a block should not happen*)
      |i::q,_->match i.op with 
        |Opn(Invoke(_),_) -> 
          let g' = Icfg.add {b_id with sub_id =count} {value=List.rev last_block;next = build_next_regular (count +1)} g in 
          let g'' = Icfg.add {b_id with sub_id = count +1} {value = [i];next=Icfg.Edg_set.union (build_next_regular (count +2)) (build_invoke i.args)} g' in 
          foo q [] g'' (count +2)
        |_->foo q (i::last_block) g count 
      in foo b [] g0 0 
  let add_method b_id dg = 
    let open Cfg in
    let open Icfg in
    let b_id_from_pos p :key = let N_id(n_id) = p.node in {b_id with n_id =n_id} in
    let g0 = Icfg.empty in
    let h = Hashtbl.create 10 in  
    let runthrough edges edg_t= 
      List.iter (
        fun (p1,p2)->let former_next=   Hashtbl.find_opt h (b_id_from_pos p1) in match former_next with 
          |None->Block_id.pp (b_id_from_pos p1); Hashtbl.add h (b_id_from_pos p1) (Edg_set.singleton {next = b_id_from_pos p2;edg_t})
          |Some(edg_s)->Hashtbl.replace h (b_id_from_pos p1) (Edg_set.add {next=b_id_from_pos p2;edg_t} edg_s)
      ) edges
    in runthrough dg.regular_edges Regular; runthrough dg.exception_edges Exception; runthrough dg.taken_edges Taken; 
    let g_final = 
    List.fold_left (
      fun g (N_id(n_id),b) ->
         let b_id' = {b_id with n_id = n_id} in 
         let next = match  Hashtbl.find_opt h b_id' with |None -> Edg_set.empty |Some(s) -> s in
         let g' = split_block b_id' next b in 
         merge g g'

    )  g0 dg.nodes in Hashtbl.clear h; g_final

  let create : Instructions.block Cfg.cfg -> t= 
    let open Cfg in 
    let open Icfg in 
    let g0 = empty in 
    List.fold_left (
      fun g (M_id(m_id),dg) -> let b_id:key= {m_id;n_id="0";sub_id=0} in 
        let g' = add_method b_id dg in 
        merge g g'
    ) g0
end



