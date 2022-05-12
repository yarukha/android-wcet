type instruction = Dvk_h.instruction

type method_descriptor = Descriptor of  string
type next_instr = MethodCall of method_descriptor | Pc_incr of int 
type type_method = (instruction * next_instr) list

let get_method m = let Descriptor(x)= m in x

type program = Empty_prog | Prog of {
  entry : method_descriptor;
  cfg : (method_descriptor,type_method) Hashtbl.t
}
