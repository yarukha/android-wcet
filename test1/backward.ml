open Apron
open Format

module Domain = Box
let man = Domain.manager_alloc ()
let x1 = Var.of_string "x1"
let x2 = Var.of_string "x2"
let env = Environment.make [|x1;x2|] [||]
module T = Int 

module P = struct 
  type property = Domain.t Abstract1.t
  let bottom :property = Abstract1.bottom man env
  let equal : property -> property -> bool= Abstract1.is_eq man
  let is_maximal : property ->bool = Abstract1.is_top man
end



module F = Fix.Fix.ForOrderedType(T)(P)


let abs_from_tag = fun x -> Abstract1.of_lincons_array man env (Parser.lincons1_of_lstring env x)
let assign_array pred var exp= Abstract1.assign_linexpr_array man pred (Array.map Var.of_string var) (Array.map (Parser.linexpr1_of_string env) exp) None
let meet = Abstract1.meet man
let join = Abstract1.join man 
let widening = Abstract1.widening man
let top = Abstract1.top man env
let bot = Abstract1.bottom man env
let (eqs : F.equations) = fun i v -> match i with 
  |2 -> abs_from_tag ["x1=0"]
  |3 -> Abstract1.widening man (v 3) (Abstract1.join man (v 2) (v 6))
  |4 -> Abstract1.meet man (v 3) (abs_from_tag ["x1<40"])
  |5 -> Abstract1.assign_linexpr man (v 4) x2 (Parser.linexpr1_of_string env "x1") None
  |7 -> Abstract1.meet man (v 3) (abs_from_tag ["x1>=40"])
  |_ -> P.bottom

let l = [1;2;3;4;5;6]
let print_valuation (v: int -> 'a Abstract1.t) = 
  List.iter (fun i -> printf "#%i: %a@." i Abstract1.print (v i)) l



let rec fp f x0 i=
let x1 = f x0 in
  printf "\niteration #%i:\n" i;
  print_valuation x0;
  if (List.map x0 l = List.map x1 l)
      then x1 else fp f x1 (Int.add i 1)

let solver (eqs:F.equations) = 
  let v0 = fun _ -> P.bottom in 
  let step v = fun i -> (eqs i) v in 
  fp step v0 0

let f () = 
  let v =  F.lfp eqs in 
  print_valuation v


