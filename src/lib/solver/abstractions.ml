open Cfg_analysis


module type Abstraction  = sig
  type concrete
  type abstract
  val interpretation : concrete -> abstract -> abstract
end


module Product_Abstraction (F1:Abstraction) (F2:Abstraction) :Abstraction = struct 
  type concrete = F1.concrete * F2.concrete
  type abstract = F1.abstract * F2.abstract
  let interpretation c a = 
    (F1.interpretation (fst(c)) (fst(a)),F2.interpretation (snd(c)) (snd(a)))
end

module Instr2block_Abstraction (F:Abstraction with type concrete = Instructions.instruction) 
  : Abstraction with type abstract = F.abstract and type concrete = Instructions.block = struct 
  type concrete = Instructions.block 
  type abstract = F.abstract
  let interpretation c a = 
    List.fold_left (fun a' c' -> F.interpretation c' a') a c
end

module IntBlockAbstraction (A: Lattices.SemiLattice with type t = int) 
  : (Abstraction with type abstract = int and type concrete = Instructions.block) = struct
    type concrete = Instructions.block
    type abstract = int 
    let interpretation _ a = 
      if A.is_maximal a then a else a+1
  end