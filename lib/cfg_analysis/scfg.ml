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