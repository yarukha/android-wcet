%{
    open Dvk

    let translate_operator op = 
        match op with 
        |_ -> failwith "unknown operator"

    let translate_operand x = 
        match x with 
        |_ -> Operand(x)

    let translate_instruction s = 
        match String.split_on_char ' ' s with 
        |[op;x1;x2]->Inst(translate_operator op, translate_operand x1,translate_operand x2)
        |_ -> failwith "wrong translation" 
        
%}

%token <string> CLASS_START
%token <string> METHOD_START
%token <string> INSTR

%token EOF

%start program 
%type <Dvk.program option> program
%%

program: 
    |p = nonempty_list(classe) EOF {Some(p)}
    |EOF {None}
    ;

classe: 
    |id = CLASS_START ; c = list(methode) {(id,c)}
    ;
    

methode:
    |id = METHOD_START ; instr = list(instruction) {(id,instr)}

instruction: 
    |instr = INSTR {translate_instruction instr}
    ;