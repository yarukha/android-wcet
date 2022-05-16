type instruction = Dvk.instruction

type block_pos = string
type block = {
  id : int;
  instructions : instruction list;
  next : block_pos list; 
}

type method_type = Empty_method | Method of {
  entry: block_pos;
  cfg : (block_pos,block) Hashtbl.t;
  exit : block_pos list
}


