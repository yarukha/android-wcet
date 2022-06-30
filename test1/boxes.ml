open Apron
open Format

module Domain = Box
let man = Box.manager_alloc ()
let vars_of_id = (Array.map (fun i -> Var.of_string (Printf.sprintf "x%i" i))) 
let env = Environment.make (vars_of_id [|1;2|]) [||]

module T = Int 

module P = struct 
  type property = Domain.t Abstract1.t
  let bottom :property = Abstract1.bottom man env
  let equal = Abstract1.is_eq man
  let is_maximal = Abstract1.is_top man
end



module F = Fix.Fix.ForOrderedType(T)(P)


let abs_from_tag = fun x -> Abstract1.of_lincons_array man env (Parser.lincons1_of_lstring env x)

let eqs : F.equations= fun i -> 
  fun v -> match i with 
  |2 -> abs_from_tag ["x1=0"]
  |3 -> Abstract1.widening man (v 3) (Abstract1.join man (v 2) (v 6))
  |4 -> Abstract1.meet man (v 3) (abs_from_tag ["x1<40"])
  |5 -> Abstract1.assign_linexpr man (v 4) (Var.of_string "x2") (Parser.linexpr1_of_string env "x1") None
  |6 -> Abstract1.assign_linexpr man (v 5) (Var.of_string "x1") (Parser.linexpr1_of_string env "x2+1") None
  |7 -> Abstract1.meet man (v 3) (abs_from_tag ["x1>=40"])
  |_ -> P.bottom

let l = [1;2;3;4;5;6;7]
let print_valuation (v: int -> 'a Abstract1.t) = 
  List.iter (fun i -> printf "x%i: %a@." i Abstract1.print (v i)) l



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
  let v =  solver eqs in 
  print_valuation v


