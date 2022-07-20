open Apron
open Format

let man = Oct.manager_alloc ()
let vars_of_id = (Array.map (fun i -> Var.of_string (Printf.sprintf "x%i" i))) 
let env = Environment.make (vars_of_id [|1;2|]) [||]

module T = Int 

module P = struct 
  type property = Oct.t Abstract1.t
  let leq_join :(property -> property -> property)= fun p q -> Abstract1.join man p q 
end

let abs_from_tag x = Abstract1.of_lincons_array man env (Parser.lincons1_of_lstring env x)
let assign pred var exp= Abstract1.assign_linexpr man pred (Var.of_string var) (Parser.linexpr1_of_string env exp) None

module G = struct 
  type variable = T.t 
  type property = P.property 
  let foreach_root contribute = 
    contribute 1 (Abstract1.bottom man env)
  let foreach_successor v p contribute = match v with 
    |1 -> contribute 2  (assign p "x1" "0")
    |2 -> contribute 3 p
    |3 -> contribute 4 (Abstract1.meet man p (abs_from_tag ["x1<40"]));
          contribute 7 (Abstract1.meet man p (abs_from_tag ["x1>=40"]))
    |4 -> contribute 5 (assign p "x2" "x1")
    |5 -> contribute 6 (assign p "x1" "x2+1")
    |6 -> contribute 3 p
    |7 -> contribute 8 (Abstract1.bottom man env)
    |_ -> ()
end

module F = Fix.DataFlow.ForOrderedType(T)(P)(G)
let l = [1;2;3;4;5;6;7]
let print_valuation_opt (v: int -> 'a Abstract1.t option) = 
  List.iter (fun i -> printf "x%i: %a@." i Abstract1.print (match v i with |None -> Abstract1.bottom man env |Some(a)->a)) l



let f () = 
  let v =  F.solution in 
  print_valuation_opt v


