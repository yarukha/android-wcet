open Apron
open Format

module Domain = Box
let man = Domain.manager_alloc ()
let x1 = Var.of_string "x1"
let x2 = Var.of_string "x2"
let env = Environment.make [|x1;x2|] [||]
let assign a v s= Abstract1.substitute_texpr man a v (Parser.texpr1_of_string env s) None
let of_l a l =(Abstract1.meet_tcons_array man a (Parser.tcons1_of_lstring env l))

module T = Int

module P = struct  
  type property = Domain.t Abstract1.t 
  let leq_join p q = 
    if Abstract1.is_leq man p q then q 
    else Abstract1.join man  p q
  end

module G = struct 
  type variable = int 
  type property = P.property
  let foreach_root contribute = 
    contribute 1 (Abstract1.top man env)
  let foreach_successor v a contribute = 
    match v with 
    |1->contribute 2 (of_l a ["x1=0"]);
    |2-> contribute 3 a;
    |3->contribute 7 (of_l a ["x1>=40"]);
        contribute 4 (of_l a ["x1<40"])
    |4->contribute 5 (Abstract1.assign_linexpr man a x1 (Parser.linexpr1_of_string env "x2+1") None)
    |5->contribute 6 (Abstract1.assign_linexpr man a x1 (Parser.linexpr1_of_string env "x2+1") None)
    |6->contribute 3 a
    |_->()
end




module F = Fix.DataFlow.ForOrderedType(T)(P)(G)
let l = [1;2;3;4;5;6;7]
let print_valuation_opt (v: int -> 'a Abstract1.t option) = 
  List.iter (fun i -> printf "x%i: %a@." i Abstract1.print (match v i with |None -> Abstract1.bottom man env |Some(a)->a)) l



let f () = 
  let v =  F.solution in 
  print_valuation_opt v


