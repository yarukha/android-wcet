type method_id = M_id of string
type node_id = N_id of string
type pc = Instructions.pc


let pp_m (M_id(s)) = 
  Printf.sprintf "%s" s

type 'a nodes = (node_id * 'a ) list 
type position = {
  node: node_id;
  pc: pc
}

type edges = (position * position) list 

type 'a digraph = {
  nodes : 'a nodes;
  regular_edges : edges ; 
  taken_edges : edges ;
  exception_edges : edges ;
} 


type 'a  cfg =  (method_id * 'a digraph)  list


let merge a b :'a cfg = 
  a@b



