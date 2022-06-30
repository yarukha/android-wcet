open Printf
open Interval
module T = Int 

type prop = interval

module P = struct 
  type property = prop
  let bottom = Bot
  let equal p q = p = q
  let is_maximal _ = false 
end






module F = Fix.Fix.ForOrderedType(T)(P)

let l = [1;2;3;4;5;6]
let print_valuation v = 
  List.iter (fun i -> printf "%i: %s\n" i (print_interval (v i))) l

let eq1 : F.equations = function 
  |2-> fun _ -> Int(0,0)
  |3-> fun v -> widen (v 3) (union (v 2) (v 5))
  |4-> fun v -> inter (v 3) (Int(min_int,39))
  |5-> fun v -> (v 4) + Int(1,1)
  |6-> fun v -> inter (v 3) (Int(40,max_int))
  |_ -> fun _ -> Int(min_int,max_int)

let rec fp f (x0:F.valuation) i=
  let x1 = f x0 in
  printf "\niteration #%i:\n" i;
  print_valuation x0;
  if (List.map x0 l = List.map x1 l)
     then x1 else fp f x1 (Int.add i 1)

let solver (eqs:F.equations) = 
  let v0: F.valuation = fun _ -> Bot in 
  let step v = fun i -> (eqs i) v in 
  fp step v0 0


let f () = 
  let v1 = F.lfp eq1 in 
  (* let v2 = solver eq1 in  *)
  printf "\nsolution: \n";
  print_valuation v1
  
