open Icfg

let transform_dg (dg:'a Cfg.digraph) :'a cfg =
  let n = List.length dg.nodes in 
  let h = Hashtbl.create n in 
  List.iter  (
    fun (n_id,x) -> Hashtbl.add h n_id {value = x;next = []} 
  ) dg.nodes;
  let update_edg (edg: Cfg.edges) t = 
    List.iter (
      fun (pi,pf)->
        let current_node = Hashtbl.find h Cfg.(pi.node) in 
        let next_id = Cfg.(pf.node) in
        Hashtbl.replace h pi.node {value = current_node.value;next = (next_id,t)::current_node.next}
    ) edg in 
  update_edg dg.regular_edges Regular;
  update_edg dg.taken_edges Taken;
  update_edg dg.exception_edges Exception;
  h




let transform_cfg (cfg:'a Cfg.cfg) :'a icfg= 
  let n= List.length cfg in 
  let h = Hashtbl.create n in 
  List.iter (
    fun (m_id,dg) -> Hashtbl.add h m_id (transform_dg dg)
  ) cfg;
  h
