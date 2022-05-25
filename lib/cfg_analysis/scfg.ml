(**s for simple*)

type method_id = Cfg.method_id
type node_id = Cfg.node_id

type label= {
  method_id : method_id;
  node_id : node_id;
}

type 'a node = {
  value : 'a ;
  next : label list
}

type 'a cfg = (label, 'a node) Hashtbl.t 

let label_to_string l = 
  let Cfg.M_id(m_id) = l.method_id and Cfg.N_id(n_id)=l.node_id in 
  Printf.sprintf "%s #%i" m_id n_id

let iter f (cfg: 'a cfg)  = 
  Hashtbl.iter (
    fun l n -> f l n.value  
  ) cfg 