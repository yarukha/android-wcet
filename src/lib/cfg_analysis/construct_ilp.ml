open Power_model
open Icfg

module type ILP_spec = sig 
  val mt : Value.t M_s.t 
  val me : Value.t M_s.t
  val given_t : Value.t
  val given_e : Value.t
end

module Build(M: ILP_spec)  = struct

  module Bt = Block_Model(T)(struct let m = M.mt let given_value = M.given_t end)
  module Be = Block_Model(E)(struct let m = M.me let given_value = M.given_e end)

  module S_key = Block_Icfg.S_Meth
  module M_key = Map.Make(Block_id)


  module Ord  = struct 
    type t = Value.t * (Block_id.t * Block_id.t)
    let compare x y = match Value.compare (fst(x)) (fst(y)) with 
      |0-> compare (snd(x)) (snd(y))
      |x -> x
  end
  module So = Set.Make(Ord)




  let reachable_blocks icfg b0 def_meths=
    let reached_h = Hashtbl.create 32 in 
    let links_h = Hashtbl.create 32 in 
    let block0 = Block_Icfg.Icfg.find_value b0 icfg in 
    let s0 = So.singleton (Bt.from_block def_meths block0,(Block_id.dummy,b0)) in
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
          let curr_t = Bt.add t (Bt.from_block def_meths block) in 
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
      
      
      
  module D = struct 
    type t =  Block_id.t * Block_id.t
    let compare = compare
  end
  module S_d = Set.Make(D)
  module M_d = Map.Make(D)  

  let create_problem icfg b0 rb def_meths= 
    let b_init = Block_id.from_meth_string "init" and b_final = Block_id.from_meth_string "final" in
    let (t_map,e_map) =
        (M_key.empty|>M_key.add b_init Bt.zero|>M_key.add b_final Bt.zero,
        M_key.empty|>M_key.add b_init  Be.zero|>M_key.add b_final Be.zero) 
      |>S_key.fold (
      fun b_id (t_m,e_m) ->
        let block = Block_Icfg.Icfg.find_value b_id icfg in 
        let b_t = Bt.from_block def_meths block and b_e = Be.from_block def_meths block in 
        (M_key.add b_id b_t t_m,M_key.add b_id b_e e_m)
      ) rb  in  
    let b_vars = 
      M_key.empty 
      |>M_key.add b_init (Lp.var ~integer:true "init")
      |>M_key.add b_final (Lp.var ~integer:true "final")
      |>S_key.fold ( 
      fun b_id m -> let s = Block_id.to_string_nice b_id in 
      M_key.add b_id (Lp.var ~integer:true s) m
    ) rb in 
    let s_d = S_key.fold (
      fun b_id s-> match Block_Icfg.split_invoke b_id icfg with 
        |None->
          let next = Block_Icfg.Icfg.find_next b_id icfg in 
          Block_Icfg.S_Edg.fold (fun edg s' -> if S_key.mem edg.next rb then S_d.add (b_id,edg.next) s' else s') next s
          |>S_d.add (b_id,b_final)
        |Some(inv,after_inv)->
          if S_key.mem inv rb then begin
            let s' = S_d.add (b_id,inv) s |> S_d.add (b_id,b_final) in 
            if S_key.mem after_inv rb then 
              let ret = Block_id.return_node inv in 
              S_d.add (ret,after_inv) s' |> S_d.add (ret,b_final)
            else s' end
          else s
      ) rb (S_d.singleton (b_init,b0)) in 
    let d_vars = S_d.fold (
      fun (b1,b2) m ->
        M_d.add (b1,b2) 
        (Lp.var ~integer:true @@ Format.sprintf "%sdd%s!" (Block_id.to_string_nice b1) (Block_id.to_string_nice b2)) m
    ) s_d (M_d.empty) in 
    let open Lp in 
    let obj = maximize (M_key.fold (
      fun b_id b_v p ->p++ (c (M_key.find b_id e_map) *~ b_v)
    )b_vars zero) in
    let time_cnstr = Cnstr.lt ~name:"time" (M_key.fold (
      fun b_id b_v p -> p ++ ( c (M_key.find b_id t_map) *~ b_v) 
    ) b_vars zero) (c Bt.given_value) in 
    let flow_cnstr = 
      let h = Hashtbl.create (2* (S_key.cardinal rb)) in 
      let add_d d d_v=
        let (b1,b2)= d in 
        match Hashtbl.find_opt h b1 with 
        |None->Hashtbl.add h b1 (zero,d_v)
        |Some(d1,d2)->Hashtbl.replace h b1 (d1, d2 ++ d_v);
        match Hashtbl.find_opt h b2 with 
        |None->Hashtbl.add h b2 (d_v,zero)
        |Some(d1,d2)->Hashtbl.replace h b2 (d1++d_v,d2) in 
      (*all blocks point to the final block*)
      M_d.iter add_d d_vars;
      Hashtbl.fold (
        fun b_id (p1,p2) l -> Cnstr.eq (M_key.find b_id b_vars) p1::Cnstr.eq (M_key.find b_id b_vars) p2::l
      ) h [] 
      in make obj (time_cnstr::flow_cnstr)




  let solve_problem ?(out=None) b_id prob=
    (match out with 
    |None->()
    |Some(out')->
      let s =  Block_id.filename b_id in 
      Lp.write ~short:true (Filename.concat out' ( s^".lp")) prob );
    match Lp_glpk.solve ~term_output:false prob with 
    |Ok(obj,_)->obj
    |Error msg -> failwith (Format.sprintf "%s for method %s" msg (Block_id.to_string ~short:false b_id))


  let debug_ilp b_id icfg def_meths = 
    let rb = reachable_blocks icfg b_id def_meths in 
    let prob = create_problem icfg b_id rb def_meths in 
    let open Format in 
    match Lp_glpk.solve prob with 
    |Ok(obj,vars)->
      printf "result for %s\n" (Block_id.method_string b_id);
      printf "For max_t=%F WCEC=%F\n\n" (Bt.given_value) obj;
      printf "with block counts:\n";
      Lp.PMap.iter (fun b v ->
        let s = (Lp.Poly.to_string ~short:true b) in 
        if (s.[String.length s -1] = '!') then () else 
          printf "%F <- %s\n" v s) vars
    |Error msg -> printf "%s for method %s" msg (Block_id.method_string b_id)

let bar total msg= 
  let open Progress.Line in 
  list [const msg;spinner (); bar total;percentage_of total] 

  let analyze_icfg ?(out=None) (icfg : Instructions.block Block_Icfg.Icfg.t) def_meths  = 
    let n = (S_key.cardinal def_meths) in 
    let module Vs=Bt.Make_Value_set(Block_id) in 
    let msg = Format.sprintf "solving %i ILP" n in 
    let (s_values,_)= Progress.with_reporter (bar n msg) (fun report ->
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
end