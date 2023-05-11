module type AbstractDomain = sig 

  type t 
  
  val top : t 
  
  val bottom : t 
  
  val join : t -> t -> t 
  
  val union : t -> t -> t
  
end 

module IntegerLattice : AbstractDomain =  struct
  type t = Bottom | Value of int | Top

  let bottom = Bottom

  let top = Top 

  let join a b = 
    match a,b with 
    | Bottom, x | x, Bottom -> x
    | Top, _ | _, Top -> Top 
    | Value x | Value y -> if x = x then Value x else Top 

  let union = join

end 

