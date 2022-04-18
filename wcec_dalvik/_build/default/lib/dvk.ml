type instruction = Inst of(string)

type id = string
type methode = id * instruction list
type classe = id * methode list
type program = classe list



let pp_instruction instruction = 
  match instruction with 
  |Inst(x)->Printf.sprintf "\t\t%s\n" x

let pp_id id = id

let pp_methode m = 
  Printf.sprintf "\tMethod %s\n%s" (pp_id (fst(m))) 
  (List.fold_left (fun s i -> s^(pp_instruction i)) "" (snd(m)))

let pp_classe c = 
  Printf.sprintf "%s\n%s" (pp_id (fst(c))) 
  (List.fold_left (fun s m -> s^(pp_methode m)) "" (snd(c)))

let pp_program (p:program) out = 
  List.iter (fun c -> Printf.fprintf out "%s" (pp_classe c)) p