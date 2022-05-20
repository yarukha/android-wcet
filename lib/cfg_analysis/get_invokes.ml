open Scfg
open Instructions

let test_invoke i = 
  match i.op with 
  |Opn(Invoke(_),_)->
    Some (List.nth i.args (List.length i.args -4))
  |_ -> None




let get (cfg:block cfg) = 
  Hashtbl.fold (
    fun _ n l -> 
      l@List.fold_left (
        fun l' i -> match test_invoke i with 
          |None -> l'
          |Some(m) -> (m)::l'
      ) [] n.value
  ) cfg []

let pp_list l = 
  List.iter (Printf.printf "%s\n") l

let pp_called_meths cfg = 
  pp_list (get cfg)