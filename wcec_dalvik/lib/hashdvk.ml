open Dvk_h

let hash_class c = 
  match c with 
  |Dvk.Empty_class -> Empty_class
  |Dvk.C(c') -> 
    let h_direct = Hashtbl.create 32 and h_virtual = Hashtbl.create 32 in 
    List.iter (
      fun m -> (match m with 
      |Dvk.Empty_method -> ()
      |Dvk.Method(m')-> Hashtbl.add h_direct m'.name m)
    ) c'.direct_methods;
    List.iter (
      fun m -> (match m with 
      |Dvk.Empty_method -> ()
      |Dvk.Method(m')->
        Hashtbl.add h_virtual m'.name m)
    ) c'.virtual_methods;
   C({
     descriptor = c'.descriptor; 
     superclass = c'.superclass;
     direct_methods = h_direct;
     virtual_methods = h_virtual
   })

let hash_program p = 
  match p with  
  |Dvk.Empty_prog -> Empty_prog
  |Dvk.Prog(p')-> 
    let h_prog = Hashtbl.create 65536 in 
    List.iter (
      fun c -> match c with 
      |Dvk.Empty_class -> ();
      |Dvk.C(c')-> Hashtbl.add h_prog c'.descriptor (hash_class c)
    )  p';
    Prog(h_prog)