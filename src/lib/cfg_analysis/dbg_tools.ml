open Instructions
open Scfg

let get_invoke  = 
  List.filter_map (
    fun x -> match x.op with 
    |Opn(Invoke(_),_)->
      Some(List.hd x.args)
    |_->None
  )

let print_invokes cfg = 
  Scfg.iter (
    fun l x -> 
      Printf.printf "method:%s \n" (Scfg.label_to_string l);
      List.iter (Printf.printf "%s |") (get_invoke x);
      Printf.printf "\n\n"
  ) cfg

let print_link_invokes cfg = 
  Scfg.iter (
    fun _ x -> 
      List.iter (
        fun m -> 
          match Hashtbl.find_opt Scfg.(cfg.edges) {Scfg.method_id = Cfg.M_id(m);Scfg.node_id = Cfg.N_id("0")} with
          |None -> Printf.printf "NOT FOUND %s\n" m
          |Some(_)->Printf.printf "FOUND %s\n" m
      )(get_invoke x)

  ) cfg

let print_block= 
  List.iter (fun i -> Printf.printf "%s |" (Instr_tools.instr_to_string i)) 

let print_cfg (cfg: block cfg)= 
  Hashtbl.iter  (
    fun label x -> 
      Printf.printf "%s: " (label_to_string label); print_block x.value;
      Printf.printf "-> ";List.iter (fun l -> Printf.printf "%s, " (label_to_string (fst(l)))) x.next;  Printf.printf "\n"
  ) cfg.edges


let found_invokes ?(found=true) cfg  = 
  let l = 
  Scfg.fold (
    fun l' _ x -> 
      List.fold_left (
        fun l'' m -> 
          match Scfg.find_opt cfg {Scfg.method_id = Cfg.M_id(m);Scfg.node_id = Cfg.N_id("0")} with
          |None -> if (not found)&& not (List.mem m l'') then m::l'' else l''
          |Some(_)->if (found)&& not (List.mem m l'') then m::l'' else l''
      ) l' (get_invoke x)
  ) cfg [] in 
  let found_s = if found then "FOUND" else "NOT FOUND" in
  List.iter (Printf.printf "%s %s\n" found_s) l;
  Printf.printf "\n%i method name have been %s in the cfg\n\n" (List.length l) found_s





let rec count_invokes l =
  match l with 
  |[] -> 0
  |x::q -> match x.op with 
    |Opn(Invoke(_),_)->1+count_invokes q
    |_-> count_invokes q

let test_invoke_gt2 cfg = 
  let x = ref [] in 
  iter (
    fun l n -> if count_invokes n > 2 then x:=l::!x
  ) cfg;
  List.iter (fun l -> Printf.printf "%s has more than 2 invokes\n" (label_to_string l)) !x

(**print if all the labels in cfg.invokes refers to invokes*)
let test_invokes_well_defined (cfg :block cfg)= 
  Printf.printf "invokes length: %i\n" (List.length cfg.invokes);
  let l = List.map (fun label -> find_opt cfg label) cfg.invokes in 
    List.iter (
      fun o -> match o with 
      |None->Printf.printf "error\n"
      |Some(b)-> match (List.hd b.value).op with |Opn(Invoke(_),_) ->Printf.printf "correct\n" |_->Printf.printf "incorrect\n"
    ) l

let print_1_size_block (cfg:block cfg) = 
  iter (
    fun _ x -> match x with  
      |[]->Printf.printf "error, empty block\n"
      |[i]->Printf.printf "%s\n" (Instr_tools.instr_to_string i) 
      |_->()
  ) cfg
  

let count_returns_block b = 
  match (List.hd (List.rev b)).op with 
  |Op0(ReturnVoid,_)|Op1(Return(_),_)-> 1
  |_->0

let count_returns_per_method cfg = 
  let h = Hashtbl.create 10000 in 
  iter (
    fun label x -> 
      let c = count_returns_block x in begin if c >0 then begin
        Printf.printf "return at %s:\n" (label_to_string label); print_block x; Printf.printf "\n" end end;
      let o = Hashtbl.find_opt h label.method_id in match o with 
      |None->Hashtbl.add h label.method_id (count_returns_block x)
      |Some(i)->Hashtbl.replace h label.method_id (i + count_returns_block x) 
  ) cfg ;
  Hashtbl.iter (
      fun Cfg.(M_id(m)) i -> if i> 1 then  Printf.printf "%i returns for %s\n" i m
  ) h