
type interval = Bot | Int of (int * int) 
let union p q = match p,q with 
  |Bot,a|a,Bot->a
  |Int(a,b),Int(c,d)->Int((min a c),(max b d))
let inter i1 i2 = match i1,i2 with 
  |Bot,_|_,Bot->Bot
  |Int(a,b),Int(c,d)->
    if max a c <= min b d then Int(max a c, min b d) else Bot

let (+) i1 i2 = match i1,i2 with 
  |Bot,_|_,Bot -> Bot
  |Int(a1,b1),Int(a2,b2)->Int(a1+a2,b1+b2)

let print_interval i = 
  match i with |Bot -> "bot" | Int(a,b) -> Printf.sprintf "[%i;%i]" a b

let widen i1 i2 = 
  let result = 
  match i1,i2 with 
    |Bot,x|x,Bot ->x
    |Int(a,b),Int(c,d)->
      let x = (if a<=c then a else min_int) and y = (if b>=d then b else max_int) in Int(x,y) 
    in 
  (* Printf.printf "widen %s %s=%s\n" (print_interval i1) (print_interval i2) (print_interval result); *)
  result
