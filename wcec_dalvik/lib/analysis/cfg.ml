type instruction = Dvk.instruction

type block_pos = int
type block = {
  pos : block_pos;
  instructions : instruction list;
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

type invoke_position = method_id * block_pos

type icfg =Empty_cfg  |Icfg of {
  entry_method : method_id;
  cfgs : (method_id,method_type) Hashtbl.t;
  return_arcs : (method_id,invoke_position) Hashtbl.t

}

