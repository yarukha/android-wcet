{
    open Lexing
    open Cfgparser

    exception SyntaxError of string

    let pp = false
    let print msg = 
        if pp then 
        Printf.printf "%s " msg else ()

    let print_nl msg = 
        if pp then Printf.printf "%s \n" msg else ()

    let print_anyway msg = 
        Printf.printf "%s \n" msg
}


let nl = ['\n']
let space = [' ' '\t' '\r']
let digit = ['0' - '9']
let number = ['0'] | (['1'-'9'] digit*)
let filename = "classes" number ".dex" |"classes.dex"
let hexadigit= ['0'-'9'] | ['a'-'f']
let adress = "0x" hexadigit+ [':']

rule token = parse 
    |nl {print_nl "";new_line lexbuf; token lexbuf}
    |space+ {token lexbuf}
    |"Processing '" (filename as f) "'..." {print "PROCESSING";PROCESSING(f)}
    |"Opened '" (filename as f) "', " {print "OPENING";OPENED(f)}
    |"DEX version '" (['0']? number as n) "'" {print "VERSION"; DEX_VERSION(int_of_string n)}
    |"digraph" {print "DIGRAPH";DIGRAPH}
    |"subgraph" {print "SUBGRAPH"; SUBGRAPH}
    |"regular_edges" {print "REGULAR_EDGES";REGULAR_EDGES}
    |"taken_edges" {print "TAKEN_EDGES";TAKEN_EDGES}
    |"exception_edges" {print "EXCEPTION_EDGES"; EXCEPTION_EDGES}
    |['#'] {print "#"; SHARP}
    |[','] {print ",";COMA}
    |[';'] {print ";" ;SEMI_COLON}
    |['='] {print "="; EQUAL}
    |['\"'] {print "\"";QUOTE}
    |"->" {print "->";ARROW}
    |['{'] {print "{";L_BRACKET}
    |['}'] {print "}";R_BRACKET}
    |['['] {print "[" ;L_SQR_BRACKET}
    |[']'] {print "]";R_SQR_BRACKET}
    |"node" (number as n) {print ("node"^n) ;NODE(n)}
    |"<p" (number as n) ['>'] {print ("p"^n);P(int_of_string n)}
    |":p" (number as n) {print ("p"^n); P(int_of_string n)}
    |"/*" space ([^ '/']* as m)space  "*/" {print m;METHOD_NAME(m)}
    |adress space  {let buf = Buffer.create 20 in lex_instruction buf lexbuf}
    |("[color=" [^ ']']+ ']') as i {print "INFO";SUBGRAPH_INFO(i)}
    |"shape" {print "shape";SHAPE}
    |"record" {print "record";RECORD}
    |"label" {print "label";LABEL}
    |"edge" {print "edge";EDGE}
    |eof {EOF}
    |_ {raise(SyntaxError("unmatched character: "^(lexeme lexbuf)))}
    
and lex_instruction buf = parse 
    |['|' '}'] {let s = Buffer.contents buf in  INSTRUCTION(s)}
    |"\\\"" {lex_string lexbuf; lex_instruction buf lexbuf}
    |'\\' ['}' '{' '<' '>' '|'] {Buffer.add_string buf (lexeme lexbuf); lex_instruction buf lexbuf}
    |[^ '\\' '|' '}']+ {Buffer.add_string buf (lexeme lexbuf); lex_instruction buf lexbuf}
    |_{raise(SyntaxError("unknown instruction character: "^(lexeme lexbuf)))}

and lex_string = parse
    |"// string@" number {}
    |_ {lex_string lexbuf}



