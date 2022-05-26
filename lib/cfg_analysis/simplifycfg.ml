open Scfg



let simplify_diagraph h (m_id,dg) =
  List.iter (
    fun n -> Hashtbl.add h {method_id = m_id; node_id=fst(n)} {value = snd(n);next = []}
  ) Cfg.(dg.nodes);
  let update_edge edg edg_type= 
  List.iter (
    fun (p1,p2)->
      let current_label = {method_id=m_id;node_id=Cfg.(p1.node)} in
      let current_node = Hashtbl.find h current_label in 
      let next_label = {method_id=m_id;node_id=Cfg.(p2.node)} in
      Hashtbl.replace h current_label {value=current_node.value;next=(next_label,edg_type)::current_node.next}
  ) edg in 
  update_edge Cfg.(dg.regular_edges) Regular;
  update_edge Cfg.(dg.taken_edges) Taken; 
  update_edge Cfg.(dg.taken_edges) Exception




let simplify (cfg:'a Cfg.cfg) = 
  let n = List.length cfg in 
  let h = Hashtbl.create (n*40) in
  List.iter (h |> simplify_diagraph ) cfg;  
  {edges = h;invokes = []}


