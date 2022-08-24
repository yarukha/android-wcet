module type Analysis_spec = sig 
  val b_entry : Block_id.t 
  val icfg : Icfg.Block_Icfg.t
end 




module S_key = Set.Make(Block_id)
module M_key = Map.Make(Block_id)
module Icfg_ = Icfg.Block_Icfg.Icfg

module type Solver = sig 
    val method_constraints : (float option * float option) M_key.t
end


module BackwardSolver(A:Analysis_spec) : Solver= struct 

  type instr_pos = Return | Some of int
  type b_instr_pos = {block:Block_id.t;pos:instr_pos} 

  module T = struct  
    type t = b_instr_pos
    let compare = compare 
  end
  module M_bi = Map.Make(T)
  module S_bi = Set.Make(T)
  type pred_graph = S_bi.t M_bi.t

  let add_pred key pred (g:pred_graph) :pred_graph= 
    match M_bi.find_opt key g with 
    |None->M_bi.add key (S_bi.singleton pred) g
    |Some(s)->M_bi.add key (S_bi.add pred s) g
  open Apron
  module Domain = Box
  let man = Domain.manager_alloc ()
  let b_init =Block_id.from_meth_string "init"
  let bi_init = {block=b_init;pos=Return}
  let blocks_set = 
    Icfg_.filter_set (Block_id.same_method A.b_entry) A.icfg
    |>S_key.add b_init
  let reg_numbers = 64

  let register_vars = 
    
    let t = Array.make reg_numbers @@ Var.of_string "" in 
    for i = 0 to (reg_numbers -1) do 
      t.(i)<-Var.of_string @@ Format.sprintf "v%i" i
    done; 
    t
  let bc_var = Var.of_string "bc"
  let var_array = Array.append register_vars [|bc_var|]
  let env = Environment.make var_array [||]

  module P = struct 
    type property = Domain.t Abstract1.t 
    let bottom = Abstract1.bottom man env
    let equal = Abstract1.is_eq man 
    let is_maximal = Abstract1.is_top man 
  end
  module F_backward = Fix.Fix.ForOrderedType(T)(P)

  let add_block_pred b_id m = 
    let b = Icfg_.find_value b_id A.icfg in 
    let rec foo l i m = 
      match l with 
      |[]->add_pred {block=b_id;pos=Return} {block=b_id;pos=Some(0)} m (*this is when we add an empty block*)
      |[_]->add_pred {block=b_id;pos=Return} {block=b_id;pos=Some(i)} m
      |_::q->foo q (i+1) (add_pred {block=b_id;pos=Some(i+1)} {block=b_id;pos=Some(i)} m)
    in foo b 0 m

  let pred_graph = 
    S_key.fold (
      fun b_id g -> 
        if b_id = b_init then M_bi.add {block=A.b_entry;pos=Some 0} (S_bi.singleton bi_init) g else
        let next = Icfg_.find_next b_id A.icfg in 
        add_block_pred b_id g 
        |>Icfg_.Edg_set.fold (
          fun edg g' -> 
            if S_key.mem edg.next blocks_set then 
            add_pred {block=edg.next;pos=Some(0)}{block=b_id;pos=Return} g'
            else g'
          ) next
    ) blocks_set M_bi.empty

(*   let top = Abstract1.top man env  *)
  let bot = Abstract1.bottom man env
(*    let meet = Abstract1.meet man  *)
  let join = Abstract1.join man 
  let join_pred v s= 
    S_bi.fold (
      fun bi -> join (v bi)
    ) s bot 
  let widening = Abstract1.widening man  
  
  let abstract bi a= 
  let inst_opt = match bi.pos with 
    |Return->None
    |Some(x)->match Icfg_.find_value bi.block A.icfg with 
            |[]->Some Instructions.undef_instr
            |b-> Some(List.nth b x) in 
  Instr2abs.transform man env inst_opt a 

  let abs_init = 
    let n = Array.length var_array in 
    let t = Array.make n (Interval.top) in 
    t.(n-1)<-Interval.of_int 0 0;
    Abstract1.of_box man env var_array t


  let equations : F_backward.equations=
    fun bi v -> 
      if bi = bi_init then abs_init
      else match M_bi.find_opt bi pred_graph with 
      |None->bot
      |Some(s)->match S_bi.cardinal s with 
        |0->failwith "no predecessor"
        |1->abstract bi (v @@ S_bi.choose s)
        |_->widening (abstract bi (v bi)) (join_pred v s) 
  let valuation = F_backward.lfp equations 

  let scarlar2float (scal:Scalar.t) = 
    match scal with 
    |Float(f)->f
    |Mpqf(m)->Mpqf.to_float m
    |Mpfrf(m)->Mpfrf.to_float m

  let abs2int abs= 
    let int = Abstract1.bound_variable man abs bc_var in 
    let low = if (Scalar.cmp_int int.inf 0) <= 0 then None else Some(scarlar2float int.inf) in
    let high = if Scalar.is_infty int.sup != 0   then None else Some(scarlar2float int.sup) in 
    low,high

  (*the constrainsts are just an inerval represented by a couple of floats
     the conversion to Lp constraints is done in Construct_ilp*)
  let method_constraints = 
    S_key.fold (
      fun b_id ->
        let a = valuation {block=b_id;pos=Return} in 
        M_key.add b_id(abs2int a) 
    ) blocks_set M_key.empty


  (**dump all constraints of the block set*)(* 
  let dump () = 
    S_key.iter (
      fun b_id-> 
        Abstract1.fdump man (valuation {block=b_id;pos=Return})
    ) blocks_set *)

end
    
module ForwardSolver(A:Analysis_spec) = struct 
  open Apron 
  module Domain = Box
  let man = Domain.manager_alloc ()

  type bi = {b_id : Block_id.t; pc: int option; inst : Instructions.instruction}
  let bi_init = {b_id= Block_id.from_meth_string "init";pc= None; inst=Instructions.undef_instr}
  let blocks_set = 
    Icfg_.filter_set (Block_id.same_method A.b_entry) A.icfg
    
  let reg_numbers = 64
  let register_vars =
    let t = Array.make reg_numbers @@ Var.of_string "" in 
    for i = 0 to (reg_numbers -1) do 
      t.(i)<-Var.of_string @@ Format.sprintf "v%i" i
    done; 
    t
  let bc_var = Var.of_string "bc"
  let var_array = Array.append register_vars [|bc_var|]
  let env = Environment.make var_array [||]


  module T = struct 
    type t = bi 
    let compare = compare
  end
  module M_bi = Map.Make(T)
  module S_bi = Set.Make(T)

  let get_head b_id= 
    match Icfg_.find_value b_id A.icfg with 
    |x::_->x
    |_->Instructions.undef_instr

  let succ_graph = 
    let rec foo b_id l g = 
      let ret_bi = {b_id;pc=None;inst=Instructions.undef_instr} in
      match l with 
      |[]->g
      |[x]->let next = 
        Icfg_.Edg_set.fold (
          fun edg s -> let b_n = edg.next in 
          if S_key.mem b_n blocks_set then
            S_bi.add {b_id=b_n;pc=Some(Instructions.get_pc @@get_head b_n);inst=get_head b_n} s else s
        ) (Icfg_.find_next b_id A.icfg) S_bi.empty in
        M_bi.add {b_id;pc=Some(Instructions.get_pc x);inst=x} (S_bi.singleton ret_bi) g 
        |>M_bi.add ret_bi next
      |x::y::q->
        let g_ = M_bi.add {b_id;pc=Some(Instructions.get_pc x);inst=x} (S_bi.singleton {b_id;pc=Some(Instructions.get_pc y);inst=y}) g in 
        foo b_id (y::q)  g_
      in
    let add_block b_id g = 
      let b = Icfg_.find_value b_id A.icfg in 
      foo b_id b g
    in
    S_key.fold add_block blocks_set M_bi.empty

  let abs_init = 
    let n = Array.length var_array in 
    let t = Array.make n (Interval.top) in 
    t.(n-1)<-Interval.of_int 0 0;
    Abstract1.of_box man env var_array t

  let abstract bi a= 
    let x = match bi.pc with |None->None|Some(_)->Some bi.inst in
    Instr2abs.transform man env x a

  module P = struct 
    type property =Domain.t Abstract1.t 
    let leq_join p q = 
      if Abstract1.is_leq man p q then q else Abstract1.join man p q
  end

  module G = struct 
    type variable = T.t 
    type property = P.property 
    let foreach_root contribute = 
      contribute bi_init abs_init
    let foreach_successor bi a contribute = 
      match M_bi.find_opt bi succ_graph with 
      |None->()
      |Some(s)->
        match S_bi.cardinal s with 
        |1->let bi = S_bi.choose s in contribute bi (abstract bi a)
        |0->()
        |_->
          S_bi.iter (fun x -> contribute x (abstract x a)) s
    end

  module F = Fix.DataFlow.ForOrderedType(T)(P)(G)

  let top = Abstract1.top man env 

  let valuation b_id = match F.solution {b_id;pc=None;inst=Instructions.undef_instr} with 
  |None->top
  |Some(a)->a

  let scarlar2float (scal:Scalar.t) = 
    match scal with 
    |Float(f)->f
    |Mpqf(m)->Mpqf.to_float m
    |Mpfrf(m)->Mpfrf.to_float m

  let abs2int abs= 
    let int = Abstract1.bound_variable man abs bc_var in 
    let low = if (Scalar.cmp_int int.inf 0) <= 0 then None else Some(scarlar2float int.inf) in
    let high = if Scalar.is_infty int.sup != 0   then None else Some(scarlar2float int.sup) in 
    low,high

  (*the constrainsts are just an inerval represented by a couple of floats
     the conversion to Lp constraints is done in Construct_ilp*)
  let method_constraints = 
    S_key.fold (
      fun b_id ->
        let a = valuation b_id in 
        M_key.add b_id(abs2int a) 
    ) blocks_set M_key.empty
end



let cnst_b forward b_id icfg = 
  let module A_spec = struct let b_entry = b_id let icfg = icfg end in 
  if forward then 
    let module Solver = ForwardSolver(A_spec) in 
    Solver.method_constraints 
else let module Solver = BackwardSolver(A_spec) in 
    Solver.method_constraints




let cnst_map ?(forward=true) icfg def_meths = 
  let n= S_key.cardinal def_meths in 
  let bar = 
    let open Progress.Line in 
    let msg = Format.sprintf "deducing constraints" in 
    list [const msg; spinner (); bar n; percentage_of n] 
  in
  let (map,_) = Progress.with_reporter bar (fun report -> 
    S_key.fold (
      fun b_id (m,i) -> 
        let cnst = cnst_b forward b_id icfg in 
        report i;
        (M_key.add b_id cnst m,i) 
    ) def_meths (M_key.empty,1)
  )
(*   let (map,_) = S_key.fold (
      fun b_id (m,i) -> 
        Format.printf "%i " i;Block_id.pp b_id;flush stdout;
        let cnst = cnst_b b_id icfg in 
        (M_key.add b_id cnst m,i+1) 
    ) def_meths (M_key.empty,1) *)
 in map