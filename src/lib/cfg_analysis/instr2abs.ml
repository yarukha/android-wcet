open Apron
open Instructions


let bc = Var.of_string "bc"
let transform man env inst_opt a=
  match inst_opt with
  |None ->Abstract1.substitute_texpr man a bc (Parser.texpr1_of_string env "bc+1") None
  |Some(inst)->begin
    match inst.op with 
    |_->Abstract1.bottom man env 
  end