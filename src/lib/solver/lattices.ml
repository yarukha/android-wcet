module type PartialOrder = sig 
  type t
  val bottom: t
  val equal : t -> t -> bool
  val is_maximal : t -> bool 
  val pp_t : t -> string
end


module type SemiLattice = sig 
  include PartialOrder
  val join : t -> t -> t 
end


module Product_Lattice (L1:SemiLattice) (L2:SemiLattice) :SemiLattice = struct 
  type t = L1.t * L2.t 
  let bottom = (L1.bottom, L2.bottom)
  let equal = (=)
  let is_maximal x = (fst(x) |> L1.is_maximal) && (snd(x) |> L2.is_maximal)
  let pp_t x= Printf.sprintf "(%s,%s)" (fst(x) |> L1.pp_t) (snd(x)|> L2.pp_t)
  let join x y = (L1.join (fst(x)) (fst(y)),L2.join (snd(x)) (snd(y)) )
end

module Make_MaxIntLattice (N : sig val n : int end) :SemiLattice= struct 
  type t = int 
  let bottom = 0 
  let equal = (=) 
  let is_maximal = (<=) N.n 
  let join x y = min N.n (max x y)
  let pp_t = string_of_int
end
