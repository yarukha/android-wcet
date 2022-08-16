open Apron
open Instructions

let catch_args = function 
  |[]->failwith "empty args"
  |[x]->(x,x,x)
  |x::y::[]->(x,y,y)
  |x::y::z::_->(x,y,z)


let sub_top man env a v = 
  Abstract1.substitute_texpr man a (Var.of_string v) Texpr1.(of_expr env (Cst(Interval(Interval.top))) ) None


let bc = Var.of_string "bc"

let transform man env inst_opt a=
  match inst_opt with
  |None ->Abstract1.substitute_texpr man a bc (Parser.texpr1_of_string env "bc+1") None
  |Some(inst)->try begin
    match inst.op with     
    |Op0(_)|Undefined->a
    |Op1(op,_)->begin 
      match op with 
      |Move(Empty)|Move(Wide)->
        let va,vb,_= catch_args inst.args in 
        Abstract1.substitute_texpr man a (Var.of_string va) (Parser.texpr1_of_string env vb) None
      |Move(_)|MoveException|MoveResult(_)->
        let v,_,_ = catch_args inst.args  in 
        sub_top man env a v
      |_->a
    end
    |Op2(op,_)->begin 
      match op with 
      (*The Not operation is not defined, due to apron not having an explicit definition for it*)
      |Const(String)|Const(Class)|ArrayLength|NewInstance|Sput(_)|Not(_)|To(_)|ConstMethodHandle|ConstMethodType->
        let v,_,_ = catch_args inst.args in 
        sub_top man env a v
      |Const(_)->
        let va,vb,_ = catch_args inst.args in 
        Abstract1.substitute_texpr man a (Var.of_string va) (Parser.texpr1_of_string env vb) None
      |Neg(_)->
        let va,vb,_ = catch_args inst.args in 
        Abstract1.substitute_texpr man a (Var.of_string va) (Parser.texpr1_of_string env ("-"^vb)) None
      (*test operators are used in AI*)
      |_->a
    end
    |Op3(op,_)->begin
      match op with 
      |InstanceOf|NewArray|Aput(_)|Iput(_)->
        let v,_,_ = catch_args inst.args in 
        sub_top man env a v
      |Cmpl(_)|Cmpg(_)|CmpLong->
        let va,vb,vc = catch_args inst.args in 
        let cmp = Coeff.s_of_int @@ Var.compare (Var.of_string vb) (Var.of_string vc) in 
        Abstract1.substitute_texpr man a (Var.of_string va) Texpr1.(of_expr env (Cst(cmp))) None
      |Arithm(arm,_)->
        let va,vb,vc =catch_args inst.args in 
        let binop = begin match arm with 
        |Add -> Some "+"|Sub->Some "-"|Mul->Some "*"|Div->Some "/"|Rem->Some "%"|_->None end in 
        begin match binop with 
        |None->sub_top man env a va
        |Some(o)->Abstract1.substitute_texpr man a (Var.of_string va) (Parser.texpr1_of_string env (vb^o^vc)) None end
      |_->a
    end
    |Opn(_)-> a
  end

  with |_->let va,vb,vc = catch_args inst.args in Printf.printf "vars: %s %s %s \nInstruction id:%i\n" va vb vc (Instr_tools.instruction_id inst);failwith "eheh" 