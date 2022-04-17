%{
    open Dvk

    let translate_instruction s = 
        
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
    |instr = INSTR {Inst(instr)}
    ;