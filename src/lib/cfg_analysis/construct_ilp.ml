open Power_model
open Icfg

module Bt = Block_Model(T)
module Be = Block_Model(E)

module S_key = Block_Icfg.S_Meth
module M_key = Map.Make(Block_id)


module Ord  = struct 
  type t = FloatValue.t * (Block_id.t * Block_id.t)
  let compare x y = match FloatValue.compare (fst(x)) (fst(y)) with 
    |0-> compare (snd(x)) (snd(y))
    |x -> x
end
module So = Set.Make(Ord)




let reachable_blocks icfg b0 def_meths=
  let reached_h = Hashtbl.create 32 in 
  let links_h = Hashtbl.create 32 in 
  let block0 = Block_Icfg.Icfg.find_value b0 icfg in 
  let s0 = So.singleton (Bt.from_block block0 def_meths,(Block_id.dummy,b0)) in
  let is_minimal k t= 
    match Hashtbl.find_opt reached_h k with 
    |None-> true
    |Some(t')->Bt.lt t t' in
  let rec foo s =
    begin 
      match So.min_elt_opt s with 
      |None-> () 
      |Some(t,(k1,k2))->
        let s' = So.remove (t,(k1,(k2))) s in
        let block = Block_Icfg.Icfg.find_value k2 icfg in 
        let next = Block_Icfg.Icfg.find_next k2 icfg in 
        let curr_t = Bt.add t (Bt.from_block block def_meths) in 
        let not_min = not (is_minimal k2 curr_t) in
        if (Bt.over_max curr_t||not_min) then 
          foo s' 
        else(
          Hashtbl.add reached_h k2 curr_t;
          if Block_id.is_return k2 then 
            match Hashtbl.find_opt links_h k2 with 
            |None-> foo s'
            |Some(after_inv)->foo (So.add (curr_t,(k2,after_inv)) s')
          else
            match Block_Icfg.split_invoke k2 icfg with 
            |None->foo (Block_Icfg.S_Edg.fold (
              fun edg s'' ->So.add (curr_t,(k2,edg.next)) s''
            ) next s')
            |Some(inv,after_inv)->
              Hashtbl.add links_h (Block_id.return_node inv) after_inv;
              foo (So.add (curr_t,(k1,inv)) s'))
      end in 
  foo s0;
  let s' = Hashtbl.fold (fun b_id _-> S_key.add b_id) reached_h (S_key.empty) in 
  s'
    
    
    
    

  
let create_problem icfg b0 rb def_meths= 
  let (t_map,e_map) = S_key.fold (
    fun b_id (t_m,e_m) -> 
      let block = Block_Icfg.Icfg.find_value b_id icfg in 
      let b_t = Bt.from_block block def_meths and b_e = Be.from_block block def_meths in 
      (M_key.add b_id b_t t_m,M_key.add b_id b_e e_m)
  ) rb (M_key.empty,M_key.empty) in 
  let (b_vars,f_vars) = S_key.fold (
    fun b_id (b_m,f_m) -> 
      let s =  Block_id.to_string_nice b_id in 
      (M_key.add b_id (Lp.var ~integer:true s) b_m,M_key.add b_id (Lp.var ~integer:true (String.cat s "f") )f_m)
  ) rb (M_key.empty,M_key.empty) in 
  let open Lp in 
  let obj = maximize (M_key.fold (
    fun b_id b_v p -> p++ (c (M_key.find b_id e_map) *~ b_v)
  )b_vars zero) in
  let time_cnstr = Cnstr.lt ~name:"time" (M_key.fold (
    fun b_id b_v p -> p ++ ( c (M_key.find b_id t_map) *~ b_v) 
  ) b_vars zero) (c Bt.given_value) in 
  let final_cnstr = Cnstr.eq ~name:"final" (M_key.fold (
    fun _ f_v p -> p ++ f_v
  )f_vars zero) one in 
  let flow_cnstr = 
    let h = Hashtbl.create (S_key.cardinal rb) in 
    let find_m b_v m = match M_key.find_opt b_v m with 
      |None->zero 
      |Some(p)->p in
    let add_c p b_id = match Hashtbl.find_opt h b_id with 
      |None->Hashtbl.add h b_id p 
      |Some(prev_p)-> Hashtbl.replace h b_id (prev_p ++ p) in 
    S_key.iter (fun b_id -> Hashtbl.add h b_id zero) rb; 
    add_c one b0;
    M_key.iter (
      fun b_id b_v -> 
        let next = Block_Icfg.Icfg.find_next b_id icfg in 
        match Block_Icfg.split_invoke b_id icfg with 
        |None->
          let out_p= Block_Icfg.Icfg.Edg_set.fold (
            fun n p -> add_c b_v n.next; p -- (find_m n.next b_vars) 
          ) next zero in 
          add_c out_p b_id;
        |Some(inv,after_inv)->
          add_c b_v inv;add_c (~-- (find_m inv b_vars)) b_id;
          let ret = Block_id.return_node inv in 
          add_c (find_m ret b_vars) after_inv;add_c (~-- (find_m after_inv b_vars)) ret 
    ) b_vars;
    Hashtbl.fold (
      fun b_id b_p l ->
        (Cnstr.eq (b_p -- (find_m b_id f_vars)) zero)::l
    ) h []
    in make obj (time_cnstr::final_cnstr::flow_cnstr)

let bar total = 
  let open Progress.Line in 
  list [spinner (); bar total;percentage_of total]



let solve_problem ?(out=None) b_id prob=
  (match out with 
  |None->()
  |Some(out')->Lp.write ~short:true (Filename.concat out' ( (String.sub (Block_id.to_string_nice b_id) 0 50)^".lp")) prob );
  match Lp_glpk.solve ~term_output:false prob with 
  |Ok(obj,_)->obj
  |Error msg -> failwith (Format.sprintf "%s for method %s\n" msg (Block_id.to_string ~short:false b_id))


let debug_ilp b_id icfg def_meths = 
  let rb = reachable_blocks icfg b_id def_meths in 
  let prob = create_problem icfg b_id rb def_meths in 
  let open Format in 
  printf "%s\n" (Lp.to_string ~short:true  prob);
  match Lp_glpk.solve prob with 
  |Ok(obj,vars)->
    printf "result for %s" (Block_id.method_string b_id);
    printf "For max_t=%F WCEC=%F\n\n" (Bt.given_value) obj;
    printf "with block counts:\n";
    Lp.PMap.iter (fun b v -> printf "%F <- %s\n" v (Lp.Poly.to_string b)) vars
  |Error msg -> printf "%s for method %s" msg (Block_id.method_string b_id)


let analyze_icfg ?(out=None) icfg def_meths  = 
  let n = (S_key.cardinal def_meths) in 
  let module Vs=Bt.Make_Value_set(Block_id) in 
  let (s_values,_)= Progress.with_reporter (bar n) (fun report ->
    S_key.fold (
      fun b_id (s,i) -> 
        let rb = reachable_blocks icfg b_id def_meths in
        let prob = create_problem icfg b_id rb def_meths in
        let obj = solve_problem ~out:out b_id prob in 
        report i;
        (Vs.add_v obj b_id s,i)
  ) def_meths (Vs.empty,1)) in
  match Vs.max_elt_opt s_values with 
  |None->Format.printf "big big failure: no ILP was solvable"
  |Some(max)-> let obj = Vs.value max and b_id = Vs.label max in 
      Format.printf "For time constraint of %f, the WCEC is reached for:\n%s\nwith a consumption of %f\n" (Bt.given_value) (Block_id.to_string b_id) obj;
     