type instruction = Dvk.instruction

type method_id = Dvk.class_descriptor * string
type next_instrs = {
  method_calls : method_id list;
  pc_incr : int list
}
type type_method = (instruction * next_instrs) list


type program = Empty_prog | Prog of {
  entry : method_id;
  cfg : (method_id,type_method) Hashtbl.t
}

let method_id_of_string s :method_id= 
  let m = Filename.extension s and 
  c = Filename.chop_extension s in 
  Dvk.Descriptor(c),m