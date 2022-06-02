(**interprocedural cfg*)

type method_id =Cfg.method_id
type node_id = Cfg.node_id
type edge_nature = Regular | Taken |Exception

type 'a node = {
  value : 'a;
  next : (node_id *edge_nature) list
}
type 'a cfg = (node_id,'a node) Hashtbl.t
type 'a icfg = (method_id,'a cfg) Hashtbl.t 


let pp_m = Cfg.pp_m

let pp_n = Cfg.pp_n

let node_numbers (cfg : 'a cfg) = 
  Hashtbl.length cfg


(**returns the list of node_id that doesnt have succesors*)
let no_successors (cfg: 'a cfg) = 
  let rec count_regular_succ l = 
    match l with 
    |[]->0
    |(_,_)::q->1+count_regular_succ q
  in
  Hashtbl.fold (
    fun n_id n l -> match count_regular_succ n.next with |0->n_id::l |_->l
  ) cfg []


let methods_count (icfg : 'a icfg)= Hashtbl.length icfg

let successors (cfg: 'a cfg) n = 
  let l = (Hashtbl.find cfg n).next in 
  List.map fst l

let pp_succ l= 
  List.fold_left (fun x (n_id,_) -> Printf.sprintf "%s;%s " x (pp_n n_id)) "" l

let predecessors (cfg :'a cfg) s = 
  Hashtbl.fold (
    fun p x l -> 
      let l' = List.fold_left (
        fun l'' (n,_)-> if n=s then p::l'' else l''
      ) [] x.next in 
      l'@l
  ) cfg []


let find_node (cfg:'a cfg) n = Hashtbl.find cfg n 
let replace_node (cfg: 'a cfg) n x =
   Hashtbl.replace cfg n x;
   cfg