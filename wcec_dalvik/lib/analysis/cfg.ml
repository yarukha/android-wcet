type instruction = Dvk.instruction

type method_descriptor = Descriptor of  string
type next_instrs = {
  method_calls : method_descriptor list;
  pc_incr : int list
}
type type_method = (instruction * next_instrs) list

let get_method m = let Descriptor(x)= m in x

type program = Empty_prog | Prog of {
  entry : method_descriptor;
  cfg : (method_descriptor,type_method) Hashtbl.t
}
