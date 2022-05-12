open Cfg
type class_method = Dvk.class_descriptor * string

let methods_number p = 
  match p with 
  |Dvk_h.Empty_prog -> 0
  |Dvk_h.Prog(hp) ->
    let n = ref 0 in 
    Hashtbl.iter (
      fun _ c -> match c with 
      |Dvk_h.Empty_class -> ()
      |Dvk_h.C(c') -> n := !n + Hashtbl.length c'.direct_methods + Hashtbl.length c'.virtual_methods
    ) hp;
  Printf.printf "methods_number: %i\n" !n;
  !n

let class_methods_of_string s :class_method= 
  let m = Filename.extension s and 
  c = Filename.chop_extension s in 
  Dvk.Descriptor(c),m

let descriptor_of_class_methods (cm:class_method) = 
  let Dvk.Descriptor(c')= fst(cm) in 
  Descriptor(c' ^"." ^snd(cm)) 



let transform_code c = match c with 
  Dvk.Empty_code -> []
  |Dvk.Code(c')->
    List.map (
      fun i -> (i, Next_instruction.next i)
    ) c'.instructions

let transform_methods c_name (m: Dvk.type_method) cfg = 
  match m with 
  |Dvk.Empty_method -> ()
  |Dvk.Method(m')->
    let new_m = transform_code m'.code in 
    Hashtbl.add cfg (descriptor_of_class_methods (c_name,m'.name)) new_m


let transform_class c cfg = 
  match c with 
  |Dvk_h.Empty_class -> ()
  |Dvk_h.C(c') -> 
    Hashtbl.iter (fun _ m -> transform_methods c'.descriptor m cfg ) c'.direct_methods;
    Hashtbl.iter (fun _ m -> transform_methods c'.descriptor m cfg ) c'.virtual_methods 

let transform_program p entry=   
  let n = methods_number p in 
  match p with 
  |Dvk_h.Empty_prog -> Empty_prog
  |Dvk_h.Prog(hp) -> 
    let cfg = Hashtbl.create n in 
    Hashtbl.iter (
      fun _ c -> transform_class c cfg;
    ) hp;
    Prog({
      entry = entry;
      cfg = cfg
    })

