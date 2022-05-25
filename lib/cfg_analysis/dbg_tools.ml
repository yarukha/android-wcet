open Instructions

let get_invoke  = 
  List.filter_map (
    fun x -> match x.op with 
    |Opn(Invoke(_),_)->
      let n = List.length x.args in Some(List.nth x.args (n-4))
    |_->None
  )

let print_invokes cfg = 
  Scfg.iter cfg (
    fun l x -> 
      Printf.printf "method:%s \n" (Scfg.label_to_string l);
      List.iter (Printf.printf "%s |") (get_invoke x);
      Printf.printf "\n\n"
  )