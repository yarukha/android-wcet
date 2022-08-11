module type Analysis_spec = sig 
  val b_entry : Block_id.t 
  val icfg : Icfg.Block_Icfg.t
end 

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


module S_key = Set.Make(Block_id)
module M_key = Map.Make(Block_id)
module I_cfg = Icfg.Block_Icfg.Icfg


module MakeSolver(A:Analysis_spec)= struct 
  open Apron
  module Domain = Oct
  let man = Domain.manager_alloc ()
  let blocks_set = 
    I_cfg.filter_set (Block_id.same_method A.b_entry) A.icfg
  
  let register_vars = 
    let t = Array.make 32 @@ Var.of_string "" in 
    for i = 0 to 31 do 
      t.(i)<-Var.of_string @@ Format.sprintf "v%i" i
    done; 
    t
  let block_vars = 
    let n = S_key.cardinal blocks_set in 
    let t = Array.make n @@ Var.of_string "" in 
    let _ = S_key.fold (
      fun b_id i ->t.(i)<- Var.of_string (Block_id.to_string ~short:true b_id);i+1) blocks_set 0 in 
    t
  let env = Environment.make (Array.append register_vars block_vars) [||]

  module P = struct 
    type property = Domain.t Abstract1.t 
    let bottom = Abstract1.bottom man env
    let equal = Abstract1.is_eq man 
    let is_maximal = Abstract1.is_top man 
  end
  module F = Fix.Fix.ForOrderedType(T)(P)

  let add_block_pred b_id m = 
    let b = I_cfg.find_value b_id A.icfg in 
    let rec foo l i m = 
      match l with 
      |[]->add_pred {block=b_id;pos=Return} {block=b_id;pos=Some(0)} m (*this is when we add an empty block*)
      |[_]->add_pred {block=b_id;pos=Return} {block=b_id;pos=Some(i)} m
      |_::q->foo q (i+1) (add_pred {block=b_id;pos=Some(i+1)} {block=b_id;pos=Some(i)} m)
    in foo b 0 m

  let pred_graph = 
    S_key.fold (
      fun b_id g -> 
        let next = I_cfg.find_next b_id A.icfg in 
        add_block_pred b_id g 
        |>I_cfg.Edg_set.fold (
          fun edg g' -> 
            if S_key.mem edg.next blocks_set then 
            add_pred {block=edg.next;pos=Some(0)}{block=b_id;pos=Return} g'
            else g'
          ) next
    ) blocks_set M_bi.empty

  let bi_init = 
    {block=A.b_entry;pos=Some(0)}

  let top = Abstract1.top man env 
  let bot = Abstract1.bottom man env
  let meet = Abstract1.meet man 
  let join = Abstract1.join man 
  let join_pred v s= 
    S_bi.fold (
      fun bi -> join (v bi)
    ) s bot 
  let widening = Abstract1.widening man 
  
  let abstract = Instr2abs.transform man env

  let equations : F.equations=
    fun bi v -> 
      if bi = bi_init then top 
      else match M_bi.find_opt bi pred_graph with 
      |None->bot
      |Some(s)->match S_bi.cardinal s with 
        |0->failwith "no predecessor"
        |1->meet (abstract bi) (v @@ S_bi.choose s)
        |_->widening (abstract bi) (join_pred v s)         
  let valuation = F.lfp equations 

  let scarlar2float (scal:Scalar.t) = 
    match scal with 
    |Float(f)->f
    |Mpqf(m)->Mpqf.to_float m
    |Mpfrf(m)->Mpfrf.to_float m

  let abs2cnst abs b_id =
    let int = Abstract1.bound_variable man abs (Var.of_string (Block_id.to_string ~short:true b_id)) in 
    (scarlar2float int.inf,scarlar2float int.sup)

  (*the constrainsts are just an inerval represented by a couple of floats
     the conversion to Lp constraints is done in Construct_ilp*)
  let block_constraints = 
    S_key.fold (
      fun b_id ->
        let a = valuation {block=b_id;pos=Return} in 
        M_key.add b_id(abs2cnst a b_id) 
    ) blocks_set M_key.empty


end
    