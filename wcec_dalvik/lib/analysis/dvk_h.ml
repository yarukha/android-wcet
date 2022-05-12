type instruction = Dvk.instruction


type class_descriptor = Dvk.class_descriptor
type type_method = Dvk.type_method

type type_class = Empty_class | C of {
  descriptor : class_descriptor; 
  superclass : class_descriptor ;
  direct_methods : (string,type_method) Hashtbl.t; 
  virtual_methods : (string,type_method) Hashtbl.t; 
}


type program = Empty_prog | Prog of ((class_descriptor,type_class) Hashtbl.t)


