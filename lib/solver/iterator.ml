open Analysis
open Cfg

module type Cfg_entry = sig
  type t
  val entry : method_id
  val icfg : t icfg
end

module type Abstraction  = sig
  type t
  val interpretation : instruction -> (t -> t)
end

module type Iterator = sig 
  type concrete
  type abstract
  val update_block: concrete -> abstract -> abstract
  val init_block : concrete block -> abstract block
  val init_method : concrete method_type -> abstract method_type
  val init_icfg : concrete icfg -> abstract icfg
  
end

module MakeIterator 
  (C: Cfg_entry with type t = (instruction list)) 
  (A:Lattices.SemiLattice) 
  (F: Abstraction with type t = A.t) = struct 
  type concrete = C.t
  type abstract = A.t
  let update_block b a= 
    List.fold_left (fun a' b' -> (F.interpretation b') a' ) a b
  let init_block b = 
    {pos = b.pos; instructions = A.bottom; next = b.next}
  let init_method m =
    match m with 
    |Empty_method -> Empty_method
    |Method(m')->
      let new_cfg = Hashtbl.create (Hashtbl.length m'.cfg) in 
      Hashtbl.iter (fun pos b -> Hashtbl.add new_cfg pos (init_block b) ) m'.cfg;
      Method {name = m'.name; entry = m'.entry; cfg = new_cfg ; exit = m'.exit; invokes = m'.invokes }
  let init_cfg c = 
    match c with
    |Empty_cfg -> Empty_cfg
    |Icfg(icfg) -> 
      let a_cfgs = Hashtbl.create (Hashtbl.length icfg.cfgs) in 
      Hashtbl.iter (
        fun id m -> Hashtbl.add a_cfgs id (init_method m)
      ) icfg.cfgs;
      Icfg {cfgs = a_cfgs; return_arcs = icfg.return_arcs}
    

end


