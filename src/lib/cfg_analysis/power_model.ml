open Instructions
open Instr_tools

module type ET = sig 
  type t 
  val range : t
  val max_v : t 
  val from_instr : instruction ->t 
  val from_native : string -> t 
end

module T : ET = struct 
  type t = float 
  let range = 1.
  let max_v = 1000.
  let from_instr instr= 
    let t= Array.make number_instructions 0. in 
    Random.init 32;
    for i = 0 to number_instructions -1 do 
      t.(i)<-Random.float range
    done;
    t.(instruction_id instr)
  let from_native _ = 50.
end

module E : ET = struct 
  type t = float 
  let range = 1.
  let max_v = 10000.
  let from_instr instr= 
    let t= Array.make number_instructions 0. in 
    Random.init 33;
    for i = 0 to number_instructions -1 do 
      t.(i)<-Random.float range
    done;
    t.(instruction_id instr)
  let from_native _ = 45.
end