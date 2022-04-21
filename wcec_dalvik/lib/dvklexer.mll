{
    open Lexing
    open Dvkparser

    exception SyntaxError of string

    let print msg = (Printf.printf "%s " msg)
    let print_nl msg = (Printf.printf "%s\n" msg)


    let catch_int_of_string s = 
        try INT(int_of_string s) with Failure(_)->BIG_INT(s)


    let remove_ext s = 
        let n = String.length s in 
        String.sub s 1 (n-2)
}

let nl = ['\n']
let space = [' ' '\t' '\r']
let digit = ['0' - '9']
let decimal_number = digit+
let sharp_id = ['#'] decimal_number
let signed_number = ['-']? decimal_number
let hex_digit = digit | ['a'-'f']
let hex_number = "0x" hex_digit+
let flag = ['A'-'Z' '_']+
let flags = ['('] (flag ([' '] flag)*)?  [')'] 
let nl_string = [^ '\n']+
let quoted_string = (['''] [^ ''' '\n']+ [''']) | (['"'] [^ ''' '\n']* ['"'])
let location = "(in " [^  '\n' ')']+ ";)"
let size = decimal_number " 16-bit code units"
let instruction = hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit [':'] nl_string
let source_value = decimal_number " (" [^ '\n' ')']+ [')']
let sci_number = ['-']? digit ['.'] digit+ ['e'] ['-' '+'] digit+ 
let float= ['-']? digit+ ['.'] digit+



rule token = parse 
    |nl { new_line lexbuf; token lexbuf}
    |space+ {token lexbuf}
    |"Processing"{skip_header 1 lexbuf}
    |"Class " sharp_id as c{print c ;CLASS}
    |"Class descriptor" {print "DESCRIPTOR"; DESCRIPTOR}
    |"Access flags" {print "ACCESS_FLAGS"; ACCESS_FLAGS}
    |"Superclass" {print "SUPERCLASS"; SUPERCLASS}
    |"Interfaces" {print "INTERFACES"; INTERFACES}
    |"Static fields" {print "STATIC"; STATIC}
    |"name" {print "NAME"; NAME}
    |"type" {print "TYPE"; TYPE}
    |"access" {print "ACCESS"; ACCESS}
    |"value" {print "VALUE"; VALUE}
    |"Instance fields" {print "INSTANCE"; INSTANCE}
    |"Direct methods" {print "DIRECT"; DIRECT }
    |"Virtual methods" {print "VIRTUAL"; VIRTUAL}
    |"code" {print "CODE"; CODE}
    |"registers" {print "REGISTERS"; REGISTERS}
    |"ins" {print "INS";INS}
    |"outs" {print "OUTS"; OUTS}
    |"insns size" {print "INSNS_SIZE";INSNS_SIZE }
    |"catches" {skip_code_end lexbuf}
    |"source_file_idx" {print "SOURCE_FILE_IDX";SOURCE_FILE_IDX}
    |"(none)" {print "NONE";NONE}
    |"null" {print "NULL";INT(0)}
    |"false" {print "FALSE";BOOL(false)}
    |"true" {print "TRUE"; BOOL(true)}
    |['-'] {print_nl "DASH"; DASH}
    |[':'] {print "COLON"; COLON}
    |quoted_string as s {print_nl (remove_ext s); STRING(remove_ext s)}
    |hex_number as h {print h; ADRESS(int_of_string h)}
    |signed_number as d {print_nl d; catch_int_of_string d}
    |flags as f {print_nl f; FLAGS( remove_ext f)}
    |sharp_id as s{print s;ID}
    |location as l {print_nl (remove_ext l); LOCATION(remove_ext l)}
    |size as s {print_nl s; SIZE(s)}
    |instruction as i {print_nl i;INSTR(i)}
    |source_value as s {print_nl s; SOURCE(s)}
    |float as f {print_nl f;FLOAT(float_of_string f)}
    |sci_number as s {print_nl s; SCI_NUMBER(s)}
    |eof {EOF}
    |_ {failwith "unmatched string"}


and skip_header i = parse 
    |nl_string {skip_header i lexbuf}
    |nl {new_line lexbuf; if i = 0 then (print_nl "HEADER";HEADER) else skip_header (i-1) lexbuf}

and skip_code_end = parse
    |nl_string {skip_code_end lexbuf}
    |nl {new_line lexbuf;skip_code_end lexbuf}
    |nl nl {new_line lexbuf; new_line lexbuf; print_nl "CODE_END";CODE_END}