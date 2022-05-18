type instruction = Dvk.instruction

type block_pos = int
type 'a block = {
  pos : block_pos;
  instructions : 'a ;
  next : block_pos list; 
}

type method_id = M_id of string

type 'a method_type = Empty_method | Method of {
  name : method_id;
  entry: block_pos;
  cfg : (block_pos,'a block) Hashtbl.t;
  exit : block_pos list;
  invokes : (block_pos,method_id) Hashtbl.t ;
}

type node_position = method_id * block_pos

(*we define a variant type for the cfg, in order to reuse this definition for the abstracted cfg*)
(*the entry point is not defined for now*)

type 'a icfg =Empty_cfg  |Icfg of {
  cfgs : (method_id,'a method_type) Hashtbl.t;
  return_arcs : (method_id,node_position) Hashtbl.t

}

