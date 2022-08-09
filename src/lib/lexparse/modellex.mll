{
    open Lexing
    open Modelparser

    exception SyntaxError of string
}

let nl = ['\n']
let space = [' ' '\t' '\r']
let invoke = "invoke-" [^ ' ' '\t' '\r']+
let registers = "\\{" [^ '\\']* "\\}"
let digit = ['0'-'9']
let value = digit+ ['.'] digit+

rule token = parse 
    |nl {new_line lexbuf; token lexbuf}
    |space {token lexbuf}
    |invoke {INVOKE}
    |registers {REGISTERS}
    |[','] space ([^ '|']+ as m) ['|'] {METHOD(m)}
    |value as v {VALUE(float_of_string v)}
    |['|'] {token lexbuf}
    |eof {EOF}
    |_ {raise(SyntaxError("unmatched character: "^(lexeme lexbuf)))}
