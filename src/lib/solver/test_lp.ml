open Cfg_analysis
open Format

module Instr_values = struct 
  let n_instr = Instr_tools.number_instructions 
  let m_t =
    let t = Array.make n_instr 0. in
    let random_interval () = 
      Random.float 1. in
    let () = Random.init 43 ;
      for i= 0 to n_instr-1 do 
        t.(i)<-random_interval ();
      done
    in t
  let m_e =
    let t1 = Array.make n_instr 0. in
    let random_interval () = 
      Random.float 1. in
    let () = Random.init 42 ;
      for i= 0 to n_instr-1 do 
        t1.(i)<-random_interval ();
      done
    in t1
  let t_max = 1000.
  let unfound_time = 10.
  let unfound_energy = 5. 
  let t_instruction i = 
    let id = Instr_tools.instruction_id i in 
    m_t.(id)
  let e_instruction i = 
    let id = Instr_tools.instruction_id i in 
    m_e.(id)
  let pp_array_float a = 
    Array.iter (printf "%f; ") a; printf "\n\n"
end

type b_id = B of (Cfg.method_id * Cfg.node_id)

module Block_id : (Set.OrderedType with type t = b_id) = struct 
  type t = b_id
  let compare b1 b2 = match b1,b2 with
    |B(Cfg.M_id(m1),Cfg.N_id(n1)),B(Cfg.M_id(m2),Cfg.N_id(n2)) -> 
      let x = String.compare m1 m2 in if x!=0 then x else 
        String.compare n1 n2 
end

module Block_id_extended = struct
  include Block_id
  let to_string b_id = 
    let B(Cfg.M_id(m),Cfg.N_id(n)) = b_id in Printf.sprintf "%s #%s" m n 
  let node_string (B(_,Cfg.N_id(n))) = sprintf "%s" n
end
let pp_b b = printf "%s\n" (Block_id_extended.to_string b)
module S = Set.Make(Block_id)
let node0 = Cfg.N_id("0")

type node = {value : Instructions.block; next : b_id list}
type new_cfg = (b_id,node) Hashtbl.t

(**does not treat invokes*)
let transform_to_new (icfg:Instructions.block Icfg.icfg) :new_cfg= 
  let h = Hashtbl.create 10000 in 
  Hashtbl.iter (
    fun m cfg -> Hashtbl.iter (
      fun n_id (n:Instructions.block Icfg.node) -> Hashtbl.add h (B(m,n_id)) {value = n.value; next = List.map (fun (n_id',_) -> B(m,n_id')) n.next}
    ) cfg
  ) icfg;
  h

(**set of all the defined methods in the bytecode*)
let defined_methods (n_cfg:new_cfg) = 
  Hashtbl.fold (
    fun b_id _ s ->
       match b_id with |B(_,n_id)->match n_id with
    |Cfg.N_id("0") -> S.add b_id s
    |_->s 
  ) n_cfg (S.empty)

(**returns the execution time of a block*)
let block_time (b:Instructions.block) s_meth =
  List.fold_left (
    fun t (i:Instructions.instruction) -> 
      let t_invoke = begin match i.op with 
      |Opn(Invoke(_),_)->let m = Cfg.M_id(List.hd i.args) in
        if S.mem (B(m,node0)) s_meth then 0. else Instr_values.unfound_time 
      |_-> 0. end
    in t +. (Instr_values.t_instruction i) +. t_invoke 
  ) 0. b

let block_energy (b:Instructions.block) s_meth =
  List.fold_left (
    fun t (i:Instructions.instruction) -> 
      let e_invoke = begin match i.op with 
      |Opn(Invoke(_),_)->let m = Cfg.M_id(List.hd i.args) in
        if S.mem (B(m,node0)) s_meth then 0. else Instr_values.unfound_energy
      |_-> 0. end
    in t +. (Instr_values.e_instruction i) +. e_invoke 
  ) 0. b


(**returns the block_id list of the defined successors of a node, including invokes*)
let next_blocks n s_meth = 
  List.fold_left (
    fun l (i:Instructions.instruction) -> match i.op with 
    |Opn(Invoke(_),_)->let b_id = B(Cfg.M_id(List.hd i.args),node0) in 
      if S.mem b_id s_meth then b_id::l else l
    |_->l
  ) n.next n.value

type t_e = {t:float;e:float} 



let dummy_b = B(Cfg.M_id("0"),Cfg.N_id("0"))
let f icfg = 
  let new_cfg = transform_to_new icfg in 
  let s_meth = defined_methods new_cfg in 
  printf "number of defined methods: %i\n\n" (S.cardinal s_meth);
  let s0 = S.empty in 
  let rec add_block b_id s t = 
    if S.mem b_id s then s else begin
      let node = Hashtbl.find new_cfg b_id in 
      let new_t = t -. block_time node.value s_meth in 
      if new_t <= 0. then s else 
        List.fold_left (
          fun s' b_id' ->add_block b_id' s' new_t 
        ) (S.add b_id s) node.next end in
  let module M = Map.Make(Block_id) in
  let problem b_j = 
    let reachable_blocks = add_block b_j s0 Instr_values.t_max in 
    let te_map = S.fold (
      fun b_id m-> let node = Hashtbl.find new_cfg b_id in 
      M.add b_id {t=block_time node.value s_meth;e=block_energy node.value s_meth} m
      ) reachable_blocks M.empty in
    let (b_vars,f_vars) = S.fold (
      fun b_id (mb,mf) -> (M.add b_id (Lp.var (String.cat "b" (Block_id_extended.node_string b_id))) mb,M.add b_id (Lp.var (String.cat "f" (Block_id_extended.node_string b_id))) mf)
    ) reachable_blocks (M.empty,M.empty) in
    let open Lp in 
    let obj = maximize (M.fold (fun b_id b_var p -> p ++ (c ((M.find b_id te_map).e) *~ b_var)) b_vars zero) in
    let time_cnstr = Cnstr.lt ~name:"time" (M.fold (fun b_id b_var p -> p ++ (c ((M.find b_id te_map).t) *~ b_var)) b_vars zero)  (c Instr_values.t_max) in
    let final_cnstr = Cnstr.eq ~name:"final" (M.fold (fun _ f_v p -> p ++ f_v) f_vars zero) one in
    let flow_cnstr = 
      let h_c = Hashtbl.create (S.cardinal reachable_blocks) in 
      S.iter (fun b_id -> Hashtbl.add h_c b_id zero) reachable_blocks;
      Hashtbl.replace h_c b_j (M.find b_j b_vars);
      S.iter (
        fun b_id -> 
          let b_p = M.find b_id b_vars in
          let next = (Hashtbl.find new_cfg b_id).next in 
          let out_p = List.fold_left (
            fun p n_id ->
              let p_next = Hashtbl.find h_c n_id in Hashtbl.replace h_c n_id (p_next ++ b_p);
              p ++ (M.find n_id b_vars)
          ) (M.find b_id f_vars) next in 
          let current_out = Hashtbl.find h_c b_id in Hashtbl.replace h_c b_id (current_out -- out_p)
      ) reachable_blocks;
      Hashtbl.fold (fun b_id p l -> (Cnstr.eq ~name:(String.cat "c" (Block_id_extended.node_string b_id)) p zero)::l ) h_c []
        in 
        make obj (time_cnstr::final_cnstr::flow_cnstr)
    in
  S.iter (
    fun b_id ->
      let prob = problem b_id in
      printf "trying to solve ILP problem generated from starting method: %s ...\n\n" (Block_id_extended.to_string b_id);
      printf "The problem is valid? %b\n" (Lp.validate prob);
      Lp.write ~short:true "truc.lp" prob;
      match Lp_glpk.solve prob with 
      |Ok(obj,_)->
        printf "max consumpion: %.2f\n" obj;
      |Error msg ->
        printf "%s\n" msg
  ) s_meth ;

