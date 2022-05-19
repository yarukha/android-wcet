open Cfg


let get_position_of_string s = 
  int_of_string ("0x"^s)



type branching = 
  |None |Return | If of int |Goto of int | Invoke of string 

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
    (*for now we ignore some invoke types and all the others multi op operators*)
    |Dvk.Invoke(invoke_type)->begin
      match invoke_type with 
      |Dvk.Interface -> None 
      |_ -> let n = List.length i.args in Invoke(List.nth i.args (n-3)) 
    end
    |_->None)


module B = Set.Make(Int)
let previous_pos pos blocks = 
  B.find_last (fun b -> b< pos) blocks

let block_spliting block pos = 
  let rec foo (l:Dvk.instruction list) stack= 
    match l with 
    |[]->failwith "not found position in block spliting"
    |x::q->
      if x.pc_pos = pos then (stack,l)
      else foo q (stack@[x])
  in let l1,l2 = foo (block.instructions) [] in 
  {pos = block.pos;instructions = l1;next=[pos]},{pos = pos; instructions = l2; next = block.next}
    

let transform_code c name= 
  match c with 
  |Dvk.Empty_code -> Empty_method
  |Dvk.Code(c') -> 
    let h = Hashtbl.create 32 in
    let invokes = Hashtbl.create 8 in 
    let block_pos = ref (B.singleton 0) in
    let return_pos = ref [] in
    let add_block (l:Dvk.instruction list) return next= 
      if List.length l = 0 then () else 
      let pos = (List.hd l).pc_pos in
        Hashtbl.add h pos {pos=pos;instructions=l;next=next};
        block_pos := B.add pos !block_pos;
        List.iter (fun x ->block_pos := B.add x !block_pos) next;
        if return then 
          return_pos:= pos::(!return_pos)
    in  
    let rec add_instructions (l:Dvk.instruction list) t= 
      match l with 
      |[]->failwith "empty instruction list on cfg construction"
      |[x]->add_block (t@[x]) (branching_value x = Return) []
      |x::q->
        match branching_value x with 
        |None->add_instructions q (t@[x])
        |Return->
          add_block (t@[x]) true [(List.hd q).pc_pos]; 
          add_instructions q [];
        |If(pos)->
          add_block (t@[x]) false [pos;(List.hd q).pc_pos];
          add_instructions q [];
        |Goto(pos)->
          add_block (t@[x]) false [pos];
          add_instructions q [];
        |Invoke(z)->
          add_block t false [(List.hd l).pc_pos];
          add_block [x] false [(List.hd q).pc_pos];
          Hashtbl.add invokes ((List.hd q).pc_pos) (M_id(z));
          add_instructions q [];
        in
    add_instructions c'.instructions [];
    (*we still need to make sure that every instruction branched by an if or goto
       will be the head of block*)
    B.iter (
      fun p -> if Hashtbl.mem h p then ()
      else
        let p_pos = previous_pos p !block_pos in
        let b1,b2 = block_spliting (Hashtbl.find h p_pos) p in
        Hashtbl.add h p_pos b1; Hashtbl.add h p b2
    ) !block_pos;
    Method {
      name = M_id(name);
      entry = 0;
      cfg = h;
      exit = !return_pos;
      invokes = invokes
    }

let transform_method m (Dvk.Descriptor(c_name))= 
  match m with 
  |Dvk.Empty_method -> Empty_method
  |Dvk.Method(m')->
    (* a method name also includes its type name*)
    let m_name = ((c_name)^"."^(m'.name)^":"^(m'.type_name)) in
    transform_code m'.code m_name



let transform_program p = 
  let add_m h m = 
    match m with |Empty_method -> ()|Method(m')->Hashtbl.add h m'.name m in
  match p with 
  |Dvk.Empty_prog -> Empty_cfg
  |Dvk.Prog(l)->
    let n = List.length l in 
    let h = Hashtbl.create (5*n) in
    List.iter (
      fun c -> match c with 
      |Dvk.Empty_class-> ()
      |Dvk.C(c')->
        let c_desc=c'.descriptor in
        List.iter (
          fun m -> let m' = transform_method m c_desc in add_m h m'
        ) c'.direct_methods;
        List.iter (
          fun m -> let m' = transform_method m c_desc in add_m h m'
        ) c'.virtual_methods;
    ) l ;
    (*we still need to add the return arcs*)
    let return_arcs = Hashtbl.create n in 
    Hashtbl.iter (
      fun invoke_id m -> match m with |Empty_method -> () 
      |Method(m')->
        Hashtbl.iter (fun pos invoked_id -> Hashtbl.add return_arcs invoked_id (invoke_id,pos)) m'.invokes;
    ) h;
    Icfg {
      cfgs = h;
      return_arcs = return_arcs
    }
