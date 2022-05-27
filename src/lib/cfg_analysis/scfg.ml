(**s for simple*)

type method_id = Cfg.method_id
type node_id = Cfg.node_id
type edge_type = Regular | Taken | Exception | Call

type label= {
  method_id : method_id;
  node_id : node_id;
}

type 'a node = {
  value : 'a ;
  next : (label * edge_type) list;
}

type 'a cfg = {
  edges : (label, 'a node) Hashtbl.t ;
  invokes : label list;
}


let label_to_string l = 
  let Cfg.M_id(m_id) = l.method_id and Cfg.N_id(n_id)=l.node_id in 
  Printf.sprintf "%s #%s" m_id n_id

(**[Scfg.iter f cfg] applies [f] to all bindings in [cfg], [f] receives the label and the value of each node as first and second argument respectivly.*)
let iter f (cfg: 'a cfg)  = 
  Hashtbl.iter (
    fun l n -> f l n.value  
  ) cfg.edges

(**[Scfg.fold f cfg init] computes [(f lN vN ... (f init l1 v1)...)], where [l1 ... lN] are the label of each nodes in [cfg], and [v1 ... dN] are the associated values.*)
let fold f (cfg:'a cfg) init = 
  Hashtbl.fold (
    fun l n a-> f a l n.value
  ) cfg.edges init

let find_opt cfg l = 
  Hashtbl.find_opt cfg.edges l

let length cfg = 
  Hashtbl.length cfg.edges