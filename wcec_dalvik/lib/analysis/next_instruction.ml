(****************************************************)
(*                                                  *)
(*                                                  *)
(*                                                  *)
(*                       TODO                       *)
(*                                                  *)
(*                                                  *)
(*                                                  *)
(*                                                  *)
(****************************************************)

open Cfg
open Dvk

let rtn_nxt = {method_calls = []; pc_incr = []}
let i_nxt i = {method_calls = []; pc_incr = [i]}

let next_op0 op = 
  match op with 
  |Nop -> i_nxt 1
  |ReturnVoid -> rtn_nxt


let next i =
  let op,_ = i in 
  match op with 
  |Name(_)-> i_nxt 1
  |Undefined -> failwith "undefined instruction"
  |_ -> {method_calls = [];pc_incr = []}