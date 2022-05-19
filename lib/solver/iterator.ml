open Analysis
open Cfg

module type Cfg_entry = sig
  type t
  val entry : method_id
  val icfg : icfg
end

module type Abstraction  = sig
  type t
  val interpretation : instruction -> (t -> t)
end



