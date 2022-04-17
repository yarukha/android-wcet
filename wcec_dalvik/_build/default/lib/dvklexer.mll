{
    open Lexing
    open Dvkparser

    exception SyntaxError of string
}

let digit = ['0' - '9']
let id = "#" digit+
let class_start= "Class " id 
let instr = ' ' [^'\n' '/' '"']*


rule token = parse 
    |['\n'] {new_line lexbuf; token lexbuf}
    |class_start as c {CLASS_START(String.sub c 6 (String.length c - 6))}
    |"Class descriptor"|"name"|"catches"|"Virtual methods" {skip lexbuf}
    |id as i {METHOD_START(i)}
    |['|']{instruction lexbuf}
    |eof {EOF}
    |_ {token lexbuf}

and skip = parse 
    |['\n'] {new_line lexbuf; skip lexbuf}
    |"Direct methods"|"insns size"|"locals"|"source_file_idx" {token lexbuf}
    |eof {failwith "unfinished skip"}
    |_ {skip lexbuf}

and instruction = parse 
    |instr as i {INSTR(String.sub i 1 (String.length i -1))}
    |eof {failwith "unfinished method"}
    |_{instruction lexbuf}