%{
    open Dvk
    
%}
%token HEADER
%token CLASS 
(*class tokens*)
%token DESCRIPTOR ACCESS_FLAGS SUPERCLASS INTERFACES STATIC_FIELDS INSTANCE_FIELDS DIRECT_METHODS VIRTUAL METHODS SOURCE_FILE_IDX
%token <string> STRING 
(*fields and methods tokens *)
%token NAME TYPE ACCESS VALUE CODE
(*code tokens*)
%token REGISTERS INS OUTS INSNS_SIZE NONE CATCHES POSITIONS

(*symbols token*)
%token <string> ADRESS
%token SHARP BAR COLON DASH NL QUOTE
%token L_BRACKET R_BRACKET
%token <int> INT
%token <string> INST
%token EOF

%start program 
%type <Dvk.program option> program
%%

program: 
    |HEADER; p = list(classe);  EOF {Some(p)}
    |EOF {None}
    ;

classe: 
    |CLASS; DASH; 
    desc = descriptor; 
    flags = flags; 
    super = super_class;
    interf = interfaces; 
    static = static_fields; 
    instance = instance_fields; 
    direct = direct_methods; 
    virt = virtual_methods; 
    source = source_file_idx {
        descriptor = desc; 
        access_flags = flags; 
        superclass = super ;
        interfaces = interf ;
        static_fields = static;  
        instance_fields = instance; 
        direct_methods = direct; 
        virtual_methods = virt; 
        source_file_idx = source;
        }
    |_ {failwith "wrong class sntax"}
    ;

descriptor:
    |DESCRIPTOR; COLON; QUOTE; s = STRING; QUOTE {Descriptor(s)}
    |_ {failwith "wrong descriptor sntax"}
    ;

flags: 
    |ACCESS_FLAGS; COLON;a =  access {a}
    |_ {failwith "wrong flags syntax"}
    ;
access:
    | a = ADRESS; L_BRACKET; s = STRING; R_BRACKET {Flags(Printf.sprintf "%s (%s)" a s)}
    |_ {failwith "wrong access syntax"}

super_class:
    |SUPERCLASS; COLON; QUOTE; s = STRING; QUOTE {Descriptor(s)}
    |_ {failwith "wrong super syntax"}
    ;

interfaces:
    |INTERFACES; DASH {Unknown}
    |_ {failwith "wrong interfaces syntax"}
    ;

static_fields: 
    |STATIC_FIELDS; DASH; f = list(fields) {f}
    |_ {failwith "wrong static syntax"}
    ;

instance_fields: 
    |INSTANCE_FIELDS; DASH; f = list(fields) {f}
    |_ {failwith "wrong instance syntax"}
    ;

direct_methods: 
    |DIRECT_METHODS; DASH; m= list(methods) {f}
    |_ {failwith "wrong direct syntax"}
    ;

virtual_methods: 
    |VIRTUAL_METHODS; DASH; m= list(methods) {f}
    |_ {failwith "wrong virtual syntax"}
    ;

source_file_idx:
    |SOURCE_FILE_IDX; COLON; INT; L_BRACKET; STRING; R_BRACKET {Unknown}
    |_ {failwith "wrong sourcefile syntax"}
    ;

fields: 
    |SHARP; INT; COLON; L_BRACKET; STRING; R_BRACKET; 
    NAME; COLON; QUOTE; name = STRING;  QUOTE; 
    TYPE; COLON; QUOTE; t = STRING; QUOTE; 
    ACCESS; COLON; a = access; 
    VALUE; COLON; i = INT 
    {{
        name = name; 
        type_value = t; 
        access = a;
        value = i
    }}
    |_ {failwith "wrong fields syntax"}
    ;

methods: 
    |SHARP; INT; COLON; L_BRACKET; STRING; R_BRACKET; 
    NAME; COLON; QUOTE; name = STRING;  QUOTE; 
    TYPE; COLON; QUOTE; t = STRING; QUOTE; 
    ACCESS; COLON; a = access; 
    c = code
    {{
        name = name; 
        type_value = t; 
        access = a;
        code = c
    }}
    |_ {failwith "wrong methods syntax"}
    ;

code: 
    |CODE; COLON; NONE {Empty_code}
    |CODE; DASH; 
    REGISTERS; COLON; r = INT; 
    INS; COLON; ins = INT; 
    OUTS; COLON; outs = INT; 
    INSNS_SIZE; COLON; size = STRING; 
    inst = list(instruction) 
    {Code({
        registers = int; 
        ins = ins; 
        ous = outs; 
        insns_size = size; 
        instructions = inst
    })}
    |_ {failwith "wrong code syntax"}
    ;

instruction: 
    |i = INST {Inst(i)}
    |_ {failwith "wrong instruction syntax"}