type class_descriptor =  Descriptor of string
type flags = Flags of int * (string list) 
type undefined_type = Unknown

type interface = Interface of string 
type value = 
  |Value_int of int | Value_string of string 
  |Value_Sci of string|Value_float of float
  |Value_bool of bool |Value_Big_int of string

type instruction = 
  Inst of string 
  |Unmatched

type position = Pos of string
type type_field = Empty_field |Field of {
  interface : interface;
  name : string; 
  type_name : string; 
  access : flags; 
  value : value
}

type type_code = Empty_code 
  |Code of {
  registers : int; 
  ins : int; 
  outs: int; 
  insns_size : string; 
  instructions : instruction list; 
}

type type_method = Empty_method | Method of {
  interface: interface;
  name: string; 
  type_name : string; 
  access : flags; 
  code : type_code
}



type type_class = Empty_class | C of {
  descriptor : class_descriptor; 
  access_flags : flags; 
  superclass : class_descriptor ;
  interfaces : interface list;
  static_fields : type_field list;  
  instance_fields : type_field list; 
  direct_methods : type_method list; 
  virtual_methods : type_method list; 
  source_file_idx : undefined_type;
}

type program = Empty_prog | Prog of (type_class list)