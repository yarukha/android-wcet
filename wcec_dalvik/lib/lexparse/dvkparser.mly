%{
    
    open Analysis
    open Dvk

    exception UnknownInstruction of string
%}
%token HEADER
%token CLASS 
(*class tokens*)
%token DESCRIPTOR ACCESS_FLAGS SUPERCLASS INTERFACES STATIC INSTANCE DIRECT VIRTUAL SOURCE_FILE_IDX
%token <string> STRING
%token <int> ADRESS  
%token <string> FLAGS 
%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token <string> SOURCE
%token <string> SCI_NUMBER
%token <string> BIG_INT
(*fields and methods tokens *)

%token ID NAME TYPE ACCESS VALUE CODE UNKNOWN
%token <string> LOCATION

(*code tokens*)

%token REGISTERS INS OUTS INSNS_SIZE CODE_END NONE
%token <string> SIZE
%token <string> INSTR

(*symbols token*)
%token DASH COLON
%token EOF

%start program 
%type <Dvk.program option> program
%%

program: 
    |HEADER ; l=list(classe); EOF {Some(Prog(l))}
    |EOF {None}

classe: 
    |CLASS DASH 
    d=descriptor f=flags super=superclass i=interfaces s=static
    inst=instance direct=direct virt=virtual_m source=source_file{C({
        descriptor = d;
        access_flags=f;
        superclass=super;
        interfaces=i;
        static_fields=s;
        instance_fields=inst;
        direct_methods= direct;
        virtual_methods = virt;
        source_file_idx=source
    })}

descriptor:
    |DESCRIPTOR COLON s = STRING {Descriptor(s)}

flags :
    |ACCESS_FLAGS COLON; a = ADRESS ; f = FLAGS {Flags(a,String.split_on_char ' ' f)}

superclass : 
    |SUPERCLASS COLON s = STRING {Descriptor(s)}

interfaces:
    |INTERFACES DASH l=list(interf) {l}

interf:
    |ID COLON s=STRING {Interface(s)}

static :
    |STATIC DASH l = list(field) {l}

field: 
    |ID COLON l = LOCATION NAME COLON n=STRING
    TYPE COLON t=STRING ACCESS COLON a=ADRESS f=FLAGS
    VALUE COLON v=value {Field({
        interface = Interface(l);
        name = n;
        type_name = t;
        access =  Flags(a,String.split_on_char ' ' f) ;
        value = v
    })}
    |ID COLON l = LOCATION NAME COLON n=STRING
    TYPE COLON t=STRING ACCESS COLON a=ADRESS f=FLAGS {Field({
        interface = Interface(l);
        name = n;
        type_name = t;
        access =  Flags(a,String.split_on_char ' ' f) ;
        value = Value_int(0)
    })}

value:
    |i=INT {Value_int(i)}
    |s=STRING {Value_string(s)}
    |s=SCI_NUMBER {Value_Sci(s)}
    |f=FLOAT {Value_float(f)}
    |b=BOOL {Value_bool(b)}
    |n=BIG_INT {Value_Big_int(n)}

instance: 
    |INSTANCE DASH l=list(field) {l}

direct:
    |DIRECT DASH l = list(methode) {l}

virtual_m:
    |VIRTUAL DASH l= list(methode) {l}

methode:
    |ID COLON l = LOCATION NAME COLON n=STRING
    TYPE COLON t=STRING ACCESS COLON a=ADRESS f=FLAGS c=code {Method({
        interface = Interface(l);
        name = n;
        type_name = t;
        access =  Flags(a,String.split_on_char ' ' f) ;
        code = c
    })}

code:
    |CODE COLON NONE {Empty_code}
    |CODE DASH 
    REGISTERS COLON r=INT INS COLON i=INT OUTS COLON o=INT INSNS_SIZE COLON s=SIZE 
    l=list(instructions) CODE_END {Code({
        registers=r;
        ins=i;
        outs= o;
        insns_size = s;
        instructions = l
    })}

instructions:
    |i=INSTR {try Catch_instructions.catch i with UnknownInstruction msg -> raise(UnknownInstruction(msg))}

source_file:
    |SOURCE_FILE_IDX COLON SOURCE {Unknown}
    |SOURCE_FILE_IDX COLON INT UNKNOWN {Unknown}