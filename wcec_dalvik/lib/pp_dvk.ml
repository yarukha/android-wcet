open Dvk
let pp_program out p = 
  let print msg = Printf.fprintf out "%s " msg in 
  let print_nl msg = Printf.fprintf out "%s\n" msg in
  let print_i i = Printf.fprintf out "%i " i in 
  let nl () = print_nl "" in
  let tab () = print "\t" in 
 
  let pp_class_descriptor d= match d with Descriptor(s)-> print s in
  let pp_flags f= match f with 
    |Flags(i,s)->Printf.fprintf out "%#x " i; List.iter print s  
  in
  let pp_undefined_type _ = print "undefined" in 
  let pp_interface i = match i with |Interface(s)-> print s in 
  let pp_value v = match v with 
    |Value_int(i)->print (string_of_int i) 
    |Value_string(s)|Value_Sci(s)|Value_Big_int(s)->print s
    |Value_float(f)->print (string_of_float f)
    |Value_bool(b)->print(string_of_bool b) 
  in
  let pp_instruction i = match i with 
  |Undefined -> print "undefined instruction"
  |Name(x) -> print x
  |Op0(_)-> print "op0"
  |Op1(_)-> print "op1"
  |Op2(_)-> print "op2"
  |Op3(_)-> print "op3"
  |Weird(_)-> print "weird"
in
  
  let pp_type_field x = match x with |Empty_field -> nl ()
  |Field(f)->
    tab ();tab ();print "interface: ";pp_interface f.interface;nl ();
    tab ();tab ();print "name: ";print_nl f.name;
    tab ();tab ();print "type: "; print_nl f.type_name;
    tab ();tab ();print "access: "; pp_flags f.access; nl ();
    tab ();tab ();print "value: "; pp_value f.value
  in 

  let pp_type_code x = match x with |Empty_code -> nl ()
  |Code(c)->
    tab ();print "registers: "; print_i c.registers; nl ();
    tab ();print "ins: "; print_i c.ins; nl ();
    tab ();print "outs: "; print_i c.outs; nl ();
    tab ();print "insns size: "; print c.insns_size; nl () ;
    ();List.iter pp_instruction c.instructions;
  in 
  
  let pp_type_method x = match x with Empty_method -> nl ();
  |Method(m)->
    tab ();tab ();print "interface: ";pp_interface m.interface;nl ();
    tab ();tab ();print "name: ";print_nl m.name;
    tab ();tab ();print "type: "; print_nl m.type_name ;
    tab ();tab ();print "access: "; pp_flags m.access; nl ();
    tab ();tab ();print "code: ";nl (); pp_type_code m.code;
  in

  let pp_type_class x = match x with |Empty_class -> nl ();
  |C(c)->
    print_nl "Class: ";
    tab ();print "Descriptor: "; pp_class_descriptor c.descriptor; nl ();
    tab ();print "Access flags: "; pp_flags c.access_flags; nl ();
    tab ();print "Superclass: "; pp_class_descriptor c.superclass;nl ();
    tab ();print "Interfaces: "; List.iter pp_interface c.interfaces;nl ();
    tab ();print "Static fields: "; List.iter pp_type_field c.static_fields; nl ();
    tab ();print "Instance_fields"; List.iter pp_type_field c.instance_fields; nl ();
    tab ();print "Direct methods"; nl (); List.iter pp_type_method c.direct_methods; nl ();
    tab ();print "Virtual methods: ";nl (); List.iter pp_type_method c.virtual_methods;nl ();
    tab ();print "source file idx: "; pp_undefined_type c.source_file_idx; nl ()
  in 
  match p with 
  |Empty_prog-> print "Empty program"
  |Prog(p')-> List.iter pp_type_class p'

