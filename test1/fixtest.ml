open Printf

type cst = Bot | Top | Cst of int 
let s_cst x = match x with |Bot -> "bot" |Top -> "top" |Cst(i) -> sprintf "%i" i
let join_cst a b = match a,b with 
  |x,y when y = x-> x
  |Bot,x|x,Bot->x
  |_->Top

let (+) a b = match a,b with 
  |Bot,_|_,Bot->Bot 
  |Top,Cst _|Cst _,Top|Top,Top-> Top 
  |Cst i1, Cst i2 -> Cst (i1 +i2)

type cst_2 = {x : cst; y  : cst}
let bot2 = {x=Bot;y=Bot}
let s_cst2 x = sprintf "x=%s ; y=%s" (s_cst x.x) (s_cst x.y)
let s_cst2_opt x = match x with |None-> "None" |Some x' -> s_cst2 x'
module T = Int 

module P = struct 
  type property = cst_2 
  let leq_join  p q = 
    {x=join_cst p.x q.x;y=join_cst p.y q.y}
end

module G = struct
  type variable = T.t 
  type property = P.property
  let foreach_root contribute = 
    contribute 1 bot2
  let foreach_successor v p contribute = 
    match v  with 
    |1->contribute 2 {x=Cst 0; y = p.y}
    |2-> contribute 3 {x= p.x ; y= Cst 10}
    |3 -> 
      if (match p.x with |Top->true |Bot->false|Cst i -> i>=100) then begin contribute 4 {x= Cst 100; y = p.y}; contribute 5 bot2 end
      else begin contribute 4 bot2;contribute 5 p end  
    |5-> contribute 6 {x = p.x; y = p.y + Cst (-3)}
    |6-> contribute 7 {x= p.x + p.y;y= p.y}
    |7->contribute 8 {x= p.x; y = p.y + Cst  3}
    |_-> ()
end

module F = Fix.DataFlow.ForOrderedType (T) (P) (G)
let sol = F.solution  

let l = [1;2;3;4;5;6;7;8]
let () = 
  List.iter (fun k -> printf "%i: %s\n" k (s_cst2_opt (sol k))) l