type instruction = Dvk.instruction

type block_pos = int
type block = {
  pos : block_pos;
  instructions : instruction list;
  next : block_pos list; 
}

type method_type = Empty_method | Method of {
  entry: block_pos;
  cfg : (block_pos,block) Hashtbl.t;
  exit : block_pos list
}


