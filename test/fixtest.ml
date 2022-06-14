module T = Int 

type interval = Bot |Int of int * int 
type t = {x : interval; y : interval}
let t_bot = {x=Bot;y=Bot}
let join_int i1 i2 = match i1,i2 with 
  |Bot,Bot->Bot
  |Bot,Int (a,b)|Int(a,b),Bot->Int(a,b)
  |Int(a,b),Int(c,d)->Int(min a c,max b d)

let add_int i1 i2 = match i1,i2 with 
  |Bot,_|_,Bot->Bot
  |Int(a,b),Int(c,d)->Int(a+c,b+d)

let geq_int i1 i2 = match i1,i2 with 
  |_,Bot->true
  |Bot,Int(_)->false 
  |Int(a,b),Int(c,d)->a <= b && c<=d

let interval_to_string i = match i with 
  |Bot->"bot"
  |Int(a,b)->Printf.sprintf "[%i,%i]" a b
let t_to_string t = Printf.sprintf "x=%s y=%s" (interval_to_string t.x) (interval_to_string t.y)
let t_opt_to_string t = match t with |None -> "None" |Some(t')-> t_to_string t'

module P = struct 
  type property = t 
  let leq_join p q = if p = q then q else  {x = join_int p.x q.y; y = join_int p.y q.y}
end

module G = struct 
  type variable = T.t 
  type property = P.property
  let foreach_root contribute = 
    contribute 1 t_bot
  let foreach_successor v p contribute = 
    match v with 
    |1->contribute 2 {x=Int(0,10);y=p.y}
    |2->contribute 3 {x =p.x;y=Int(100,100)}
    |3->  contribute 4 (if geq_int p.x (Int(0,0)) then p else t_bot);
        contribute 6 (if not (geq_int p.x (Int(0,0))) then p else t_bot)
    |4->contribute 5 {x= add_int p.x (Int(-1,-1));y=p.y}
    |5-> contribute 3 {x=p.x;y=add_int (Int(10,10)) p.y}
    |_ -> ()
end

module F = Fix.DataFlow.ForOrderedType (T) (P) (G)

let f = F.solution 
let l = [1;2;3;4;5;6] 
let () = 
  List.iter (fun k ->Printf.printf "%i: %s\n" k (t_opt_to_string (f k))) l