

let methods_number p = 
  match p with 
  |Dvk_h.Empty_prog -> 0
  |Dvk_h.Prog(hp) ->
    let n = ref 0 in 
    Hashtbl.iter (
      fun _ c -> match c with 
      |Dvk_h.Empty_class -> ()
      |Dvk_h.C(c') -> n := !n + Hashtbl.length c'.direct_methods + Hashtbl.length c'.virtual_methods
    ) hp;
  Printf.printf "Methods_number: %i\n" !n;
  !n

let get_position_of_string s = 
  int_of_string ("0x"^s)



type branching = 
  |Return | If of int |Goto of int | Invoke of string |None

let branching_value (i:Dvk.instruction) =
  match i.op with 
  |Dvk.Undefined-> None
  |Dvk.Op0(op,_)->(
    match op with 
    |Dvk.ReturnVoid->Return
    |_->None)
  |Dvk.Op1(op,_)-> (
    match op with 
    |Dvk.Return(_)->Return
    |Dvk.Goto -> Goto(get_position_of_string (List.hd i.args))
    |_->None)
  |Dvk.Op2(op,_)-> (
    match op with
    |Dvk.Ifz(_)->If(get_position_of_string (List.nth i.args 1))
    |_ -> None)
  |Dvk.Op3(op,_)-> (
    match op with 
    |Dvk.If(_)->If(get_position_of_string (List.nth i.args 2))
    |_->None)
  |Dvk.Opn(op,_)->(
    match op with 
    (*very cheeky trick: the method name is located before "//" and "method@" in the list of args*)
    |Dvk.Invoke(_)->let n = List.length i.args in Invoke(List.nth i.args (n-3)) 
    |_->None)

(*
module B = Set.Make(Int)


let transform_code c name = 
  match c with 
  |Dvk.Empty_code -> Empty_method
  |Dvk.Code(c') -> 
    let h = Hashtbl.create 32 in
    let block_pos = ref B.empty in
    let return_pos = ref B.empty in
    let add_block l pos = 
      Hashtbl.add h pos l;
      block_pos := B.add pos !block_pos in
    let add_return pos = 
      return_pos := B.add pos !return_pos
    in  
    let rec add_instructions (l:Dvk.instruction list) stack= 
      match l with 
      |[] -> stack
      |x::q -> 
        let y = x.op in 
*)