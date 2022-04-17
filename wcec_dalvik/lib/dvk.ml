type operator = Unknown | Operator of string
type operand = Nil | Operand of string

type instruction = Inst of(operator * operand * operand )

type id = string
type methode = id * instruction list
type classe = id * methode list
type program = classe list



let pp_operator op = 
  match op with 
  |Unknown-> Printf.sprintf "unknown operator"  
  |Operator(o)->Printf.sprintf "%s" o

let pp_operand x = 
  match x with
  |Nil -> Printf.sprintf ""
  |Operand(o)->Printf.sprintf "%s" o 

let pp_instruction instruction = 
  match instruction with 
  |Inst(operator,op1,op2)->Printf.sprintf "\t\t%s %s %s\n" (pp_operator operator) (pp_operand op1) (pp_operand op2) 

let pp_id id = id

let pp_methode m = 
  Printf.sprintf "\tMethod %s\n%s" (pp_id (fst(m))) 
  (List.fold_left (fun s i -> s^(pp_instruction i)) "" (snd(m)))

let pp_classe c = 
  Printf.sprintf "%s\n%s" (pp_id (fst(c))) 
  (List.fold_left (fun s m -> s^(pp_methode m)) "" (snd(c)))

let pp_program p out = 
  List.iter (fun c -> Printf.fprintf out "%s" (pp_classe c)) p