type instruction = Dvk.instruction

type block_pos = int
type block = {
  pos : block_pos;
  instructions : instruction list ;
  next : block_pos list; 
}

type method_id = M_id of string

type method_type = Empty_method | Method of {
  name : method_id;
  entry: block_pos;
  cfg : (block_pos,block) Hashtbl.t;
  exit : block_pos list;
  invokes : (block_pos,method_id) Hashtbl.t ;
}

type node_position = method_id * block_pos
let string_of_node_position (M_id(m_id),b_pos) = 
  Printf.sprintf "%s #%i" m_id b_pos;

(*we define a variant type for the cfg, in order to reuse this definition for the abstracted cfg*)
(*the entry point is not defined for now*)

type icfg =Empty_cfg  |Icfg of {
  cfgs : (method_id,method_type) Hashtbl.t;
  return_arcs : (method_id,node_position) Hashtbl.t

}

