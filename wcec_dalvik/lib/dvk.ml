type class_descriptor =  Descriptor of string
type flags = Flags of string 
type undefined_type = Unknown

type instruction = 
  Inst of string 
  |Unmatched

type position = Pos of string
type type_field = {
  name : string; 
  type_value : string; 
  access : flags; 
  value : int
}
type type_code = Empty_code 
  |Code of {
  registers : int; 
  ins : int; 
  outs: int; 
  insns_size : string; 
  instructions : instruction list; 
}

type type_method = {
  name: string; 
  type_value : string; 
  access : flags; 
  code : type_code
}



type type_class = {
  descriptor : class_descriptor; 
  access_flags : flags; 
  superclass : class_descriptor ;
  interfaces : undefined_type ;
  static_fields : type_field list;  
  instance_fields : undefined_type; 
  direct_methods : type_method list; 
  virtual_methods : type_method list; 
  source_file_idx : undefined_type;
}

type program = type_class list