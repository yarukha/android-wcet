open Analysis



let cfg_method_number p = 
  match p with 
  |Cfg.Empty_cfg -> 0
  |Cfg.Icfg(cfg)->Hashtbl.length cfg.cfgs

let multiple_returns_count p = 
  match p with 
  |Cfg.Empty_cfg -> 0
  |Cfg.Icfg(cfg)->
    let i = ref 0 in 
    let names = ref [] in
    Hashtbl.iter (
      fun id m -> match m with |Cfg.Empty_method -> ()
      |Method(m')-> if List.length (m'.exit) > 1  then begin incr(i); names:= (id::(!names)) end
    ) cfg.cfgs;
    List.iter (fun (Cfg.M_id(n)) -> Printf.printf "%s has multiple returns\n"n) !names;
    !i

