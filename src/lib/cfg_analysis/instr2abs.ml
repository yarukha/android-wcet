open Apron

let bc = Var.of_string "bc"
let transform man env icfg b_id pos a=
  let _ = icfg and _ = (b_id,pos,a) in 
  match pos with 
  |None ->Abstract1.substitute_texpr man a bc (Parser.texpr1_of_string env "bc+1") None
  |_->Abstract1.top man env 
