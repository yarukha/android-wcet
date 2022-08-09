let max_string_size = 230

type t = {m_id : string; n_id: string; sub_id : int}
let compare  =compare
let short_printing = false
let from_meth_string m_id = {m_id;n_id = "0";sub_id=0}
let method_string b= b.m_id
let  node_string b = b.n_id
let subnode_string b = Format.sprintf "%i" b.sub_id
let create = 
  let open Cfg in fun (M_id(m_id)) (N_id(n_id)) sub_id -> {m_id;n_id;sub_id}
let to_string ?(short=short_printing) b =
  if short then
    Format.sprintf "n%ss%i" b.n_id b.sub_id
  else
    Format.sprintf "%s n%s s%i" b.m_id b.n_id b.sub_id

(**string of block_id nicely formated for LP variable string*)
let to_string_nice b = 
  let s = to_string ~short:false b in 
  String.map (function |' '|':'|'<'|'>'|'['|']'->'_'|c->c) s

(**returns a string that can be used as a filename suffix*)
let filename b_id = 
  let s = to_string_nice b_id in 
  if String.length s > max_string_size then String.sub s 0 max_string_size 
  else s

let pp b = Format.printf "%s\n" (to_string b)
let dummy = from_meth_string ""

let is_method_entry b_id = 
  b_id.n_id = "0" && b_id.sub_id= 0
let return_node b_id = 
  {b_id with n_id="return";sub_id=0}
let is_return b_id = 
  b_id.n_id="return" && b_id.sub_id=0
let same_method b_id1 b_id2 = 
  b_id1.m_id = b_id2.m_id

