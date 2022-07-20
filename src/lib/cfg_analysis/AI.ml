

module type Analysis_spec = sig 
  val b_entry : Block_id.t 
  val icfg : Icfg.Block_Icfg.t
end 

module MakeSolver(A:Analysis_spec)= struct 
  open Apron
  module Domain = Oct
  module Instr_pos = struct 
    type t = {block: Block_id.t; pos :int}
    let compare = compare
    let to_string b = Format.sprintf "%sp%i" (Block_id.to_string b.block) b.pos
  end
  module S_key = Set.Make(Block_id)
  module S_reg = Set.Make(String)
  let s_blocks = Icfg.Block_Icfg.method_set A.b_entry A.icfg
  let s_regs = S_reg.empty 
  let man = Domain.manager_alloc ()
  let env = 
    let a = Array.make (S_key.cardinal s_blocks + S_reg.cardinal s_regs) (Var.of_string "") in 
    let i1 = S_key.fold (fun b_id i -> a.(i)<- Var.of_string (Block_id.to_string_nice b_id);i+1) s_blocks 0 in 
    let _ = S_reg.fold (fun s i -> a.(i)<-Var.of_string s;i+1) s_regs i1 in 
    Environment.make a [||]
  module Properties = struct 
    type property = Domain.t Abstract1.t 
    let bottom = Abstract1.bottom man env
    let equal = Abstract1.is_eq man 
    let is_maximal = Abstract1.is_top man 
  end
  module Solver = Fix.Fix.ForOrderedType(Instr_pos)(Properties)
  module M_pos = Map.Make(Instr_pos)
  let incr_bc b_id =
    let s = Block_id.to_string b_id in  Format.sprintf "%s=%s+1" s s
  let m_pos = S_key.fold (
    fun b_id m ->
      let m',i =Icfg.Block_Icfg.Icfg.find_value b_id A.icfg 
      |> List.fold_left (
        fun (m',i) instr -> (M_pos.add {block = b_id;pos=i} (Instr2expr.transform instr) m',i+1)
        ) (m,1) in 
      M_pos.add {block=b_id;pos=i} (incr_bc b_id) m'
  ) s_blocks M_pos.empty
  let abs_from_string s = 
    Abstract1.of_lincons_array man env (Parser.lincons1_of_lstring env [s])
  let equations :Solver.equations= 
    fun k v -> let _ = v in match M_pos.find_opt k m_pos with 
    |Some(s)->abs_from_string s
    |None->Properties.bottom 
end
    