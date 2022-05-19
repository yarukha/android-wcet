open Analysis
open Dvk

module type Abstraction  = sig
  type concrete
  type abstract
  val interpretation : concrete -> (abstract -> abstract)
end


module type AbstractionMatrix = sig 
  type concrete 
  type abstract 
  type t = (abstract -> abstract) array
  val matrix : t
  val is_correct : t -> bool
end

module MakeAbstraction (M: AbstractionMatrix with type concrete = operator) :Abstraction= struct
  type concrete = M.concrete
  type abstract = M.abstract
  let interpretation i = 
    M.matrix.(instruction_id i)
  end