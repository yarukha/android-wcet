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
          match Hashtbl.find_opt cfg {Scfg.method_id = Cfg.M_id(m);Scfg.node_id = Cfg.N_id(0)} with
          |None -> Printf.printf "NOT FOUND %s\n" m
          |Some(_)->Printf.printf "FOUND %s\n" m
      )(get_invoke x)

  ) cfg