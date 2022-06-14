open Cfg_analysis
open Lattices

type interval = float*float
let max_loop = 10
let max_f = 1000.
let n_instr = Instr_tools.number_instructions 
let m_t = Array.make n_instr (0.,0.) 
let m_e = Array.make n_instr (0.,0.)  


let random_interval () = 
  let x = Random.float 1. and y = Random.float 1. in 
  (min x y, max x y )

let () = Random.init 42 ;
  for i= 0 to n_instr-1 do 
    m_t.(i)<-random_interval ();
    m_e.(i)<-random_interval ()
  done

let plus_interval i1 i2 = 
  (fst(i1)+.fst(i2),snd(i1)+.snd(i2)) 

module FloatIntervalLattice : SemiLattice with type t = interval =  struct
  type t = interval
  let bottom = (0.,0.)
  let equal = (=)
  let is_maximal x = snd(x) > max_f
  let pp_t x = Printf.sprintf "[%f;%f]" (fst(x)) (snd(x))
  let join x1 x2 = 
    let l1 = fst(x1) and l2 = snd(x1) and r1 = fst(x2) and r2 = snd(x2) in 
    (min l1 r1,max l2 r2)

end

module FloatIntervalAbs (M: sig val m : interval array end) : (Abstractions.Abstraction with type abstract = interval and type concrete = Instructions.instruction) 
  = struct 
  type concrete = Instructions.instruction
  type abstract = interval
  let interpretation c a = 
    let id = Instr_tools.instruction_id c in if id>n_instr then Printf.printf "error: %i %s\n" id (Instr_tools.instr_to_string c) else ();
    plus_interval M.m.(id) a
end

module Abs_t_instr = FloatIntervalAbs(struct let m = m_t end)
module Abs_e_instr = FloatIntervalAbs(struct let m = m_e end)
module Abs_t_block = Abstractions.Instr2block_Abstraction (Abs_t_instr)
module Abs_e_block = Abstractions.Instr2block_Abstraction (Abs_e_instr)

module MaxLoopLattice : SemiLattice with type t = int= Make_MaxIntLattice (struct let n = max_loop end)
module MaxLoopAbs = Abstractions.IntBlockAbstraction (MaxLoopLattice) 

type t3 = {t : interval; e : interval; n : int}

module Lattice3  : SemiLattice with type t = t3 = struct 
  type t = t3
  let bottom = {t = FloatIntervalLattice.bottom;e=FloatIntervalLattice.bottom; n=MaxLoopLattice.bottom}
  let equal x1 x2 = (x1.n = x2.n)
  let is_maximal x = MaxLoopLattice.is_maximal x.n
  let pp_t x = 
    Printf.sprintf "t=%s e=%s n=%s" (FloatIntervalLattice.pp_t x.t) (FloatIntervalLattice.pp_t x.e) (MaxLoopLattice.pp_t x.n)
  let join x1 x2 = 
    {t= FloatIntervalLattice.join x1.t x2.t; e = FloatIntervalLattice.join x1.e x2.e;n = MaxLoopLattice.join x1.n x2.n}
end


module Abs3_block : Abstractions.Abstraction with type abstract = t3  and type concrete = Instructions.block = struct
  type concrete = Instructions.block
  type abstract = t3
  let interpretation c a = 
    {
      t = Abs_t_block.interpretation c a.t;
      e = Abs_e_block.interpretation c a.e;
      n = MaxLoopAbs.interpretation c a.n
    }
end

module It : Iterator.Iterator with type concrete = Instructions.block and type abstract = t3  = Iterator.MakeIterator (Lattice3) (Abs3_block)


let cfg_abstract_value (cfg: Instructions.block Icfg.cfg) = 
  let acfg = It.init cfg in 
  let acfg = It.iterate cfg acfg in 
  It.final_value acfg


let invoke_option (i:Instructions.instruction) = 
  match i.op with 
  |Opn(Invoke(_),_)->Some(Cfg.M_id(List.hd i.args))
  |_->None

  let get_invokes_block (b:Instructions.block) = 
  List.filter_map invoke_option b

let get_invokes_cfg (cfg : Instructions.block Icfg.cfg) = 
  Hashtbl.fold (
    fun _ x l -> (get_invokes_block Icfg.(x.value))@l
  ) cfg [] 

type t2 = {t: interval; e: interval}

let max_interval i1 i2 :interval = 
  let m =  max (snd(i1)) (snd(i2)) in if snd(i1) = m then i1 else i2
let t3_to_t2 (t: t3) = 
  {t=t.t;e=t.e}

let (+) t1 t2 = 
  {t=plus_interval t1.t t2.t;e=plus_interval t1.e t2.e}
let max_t = 50.
let unfound_method_value = {t=(10.,10.);e=(9.,9.)}
let t2_init = {t=(0.,0.);e=(0.,0.)}

type alpha = {final_value: t2; next  : Cfg.method_id list}
type theta = {m_id : Cfg.method_id; curr_te: t2}

let find_max_e h m_id = 
  let rec foo l curr_max  = 
    match l with 
    |[]->curr_max
    |theta::q->
      let alpha = 
      match Hashtbl.find_opt h theta.m_id with 
      |None->{final_value= unfound_method_value;next=[]}
      |Some(x)->x 
      in 
      let new_te = theta.curr_te+ alpha.final_value in 
      if snd(new_te.t) > max_t then foo q curr_max 
      else
        let new_max = max_interval new_te.e curr_max in 
        let l' = List.map (fun m' -> {m_id=m';curr_te=new_te}) alpha.next in 
        let new_l = l' @q in 
        foo new_l new_max
  in foo [{m_id;curr_te=t2_init}] (0.,0.)
      


  

let big_time (icfg : Instructions.block Icfg.icfg) = 
  let n = Icfg.methods_count icfg in 
  let h = Hashtbl.create n in 
  Hashtbl.iter (
    fun m_id cfg -> Hashtbl.add h m_id {final_value =t3_to_t2 (cfg_abstract_value cfg); next = get_invokes_cfg cfg} 
  ) icfg ;
  let max_e = Hashtbl.create n in 
  let i = ref 1 in
  Hashtbl.iter (fun m _ -> let max_int_e = find_max_e h m in (*Printf.printf "%i: \n%s\n" !i (FloatIntervalLattice.pp_t max_int_e);*)incr(i);
    Hashtbl.add max_e m max_int_e) h;
  (* Printf.printf "result: %s\n" (FloatIntervalLattice.pp_t (Hashtbl.fold (fun _ m m'-> max_interval m m' ) max_e (0.,0.))) *)




