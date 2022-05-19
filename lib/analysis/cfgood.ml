(**simple and iterator friendly representation of a cfg*)

type node_position = Cfg.node_position 

(* a node is simply a value and the positions of its successors*)
type 'a node = Node of 'a * (node_position list)

type 'a cfg = Empty_cfg |Cfg of (node_position,'a node) Hashtbl.t