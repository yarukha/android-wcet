open Wcec_dalvik

let pp = false

let () = 
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  let p = Loaddvk.prog lb in
  Printf.printf "Parsing done\n";
  if pp then 
  let out_file = "dvk.cdvk" in 
  let out = open_out out_file in 
  close_in c;
  Pp_dvk.pp_program out p
  else ();
  let hp = Hashdvk.hash_program p in 
  Printf.printf "Hashing done\n";
  let hp' = match hp with 
  |Dvk_h.Empty_prog -> Hashtbl.create 0 
  |Dvk_h.Prog(x) -> x 
  in let n_class = Hashtbl.length hp' in 
  let (n_direct,n_virtual) = 
  let i = ref 1 in 
  Hashtbl.fold (
    fun _ c (n:(int*int)) -> 
      match c with 
      |Dvk_h.Empty_class -> n
      |Dvk_h.C(c')->
        let Dvk.Descriptor(name)= c'.descriptor in Printf.printf "Class #%i: %s\n" !i name; incr(i);
        (fst(n) + Hashtbl.length c'.direct_methods,snd(n) + Hashtbl.length c'.virtual_methods)
  ) hp' (0,0) in
  Printf.printf "number of classes: %i\n" n_class;
  Printf.printf "number of direct methods: %i\nnumber of virtual methods: %i\n" n_direct n_virtual;
