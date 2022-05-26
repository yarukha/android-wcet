open Instructions

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
          match Hashtbl.find_opt Scfg.(cfg.edges) {Scfg.method_id = Cfg.M_id(m);Scfg.node_id = Cfg.N_id(0)} with
          |None -> Printf.printf "NOT FOUND %s\n" m
          |Some(_)->Printf.printf "FOUND %s\n" m
      )(get_invoke x)

  ) cfg


let found_invokes ?(found=true) cfg  = 
  let l = 
  Scfg.fold (
    fun l' _ x -> 
      List.fold_left (
        fun l'' m -> 
          match Scfg.find_opt cfg {Scfg.method_id = Cfg.M_id(m);Scfg.node_id = Cfg.N_id(0)} with
          |None -> if (not found)&& not (List.mem m l'') then m::l'' else l''
          |Some(_)->if (found)&& not (List.mem m l'') then m::l'' else l''
      ) l' (get_invoke x)
  ) cfg [] in 
  let found_s = if found then "FOUND" else "NOT FOUND" in
  List.iter (Printf.printf "%s %s\n" found_s) l;
  Printf.printf "\n%i method name have been %s in the cfg\n\n" (List.length l) found_s;