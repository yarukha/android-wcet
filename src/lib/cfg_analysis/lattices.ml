module type AbstractDomain = sig 

  type t 
  
  val top : t 
  
  val bot              : t 
  
  val join : t -> t -> t 
  
  val union : t -> t -> t

  (*val compare: t -> t -> int*)
  
end 


module MakeSimpleLattice (T: sig type t end) : AbstractDomain = 
struct

  type t = Bot | Value of T.t | Top

  let bot = Bot

  let top = Top 

  let join a b = 
    match a,b with 
    | Bot, x | x, Bot -> x
    | Top, _ | _, Top -> Top 
    | Value x , Value y -> if x = y then Value x else Top 

  let union = join

  (*let compare a b =
      match a, b with
      | Bottom, Bottom | Top, Top -> 0
      | Bottom, _ | _, Top -> -1
      | _, Bottom | Top, _ -> 1
      | Value x, Value y -> compare x y*)
end 

module StringSetDomain : AbstractDomain = struct
  module StringSet = Set.Make(String)

  type t = Bot | StrSet of StringSet.t | Top
  let bot = Bot
  let top = Top
  let join a b = match a, b with
    | Bot, x | x, Bot -> x
    | StrSet x, StrSet y -> StrSet (StringSet.union x y)
    | _, _ -> Top
  let union = join
end



module IntegerLattice : AbstractDomain = MakeSimpleLattice (struct type t = int end)
module FloatLattice : AbstractDomain = MakeSimpleLattice (struct type t = float end)
