%{
    open Cfg_analysis
    open Cfg
%}
%token <string> PROCESSING
%token <string> OPENED 
%token <int> DEX_VERSION
%token DIGRAPH SUBGRAPH 
%token REGULAR_EDGES TAKEN_EDGES EXCEPTION_EDGES

%token SHARP COMA SEMI_COLON EQUAL QUOTE ARROW 
%token L_BRACKET R_BRACKET L_SQR_BRACKET R_SQR_BRACKET

%token <int> NODE
%token <int> P
%token <string> METHOD_NAME
%token <string> INSTRUCTION
%token <string> SUBGRAPH_INFO

%token SHAPE
%token RECORD

%token LABEL EDGE 

%token EOF 

%start cfg
%type < Instructions.instruction list cfg option> cfg
%%

cfg:
    |header l = list(digraph) EOF {Some l}
    |EOF {None}

header: 
    |PROCESSING OPENED DEX_VERSION {}

digraph: 
    |DIGRAPH L_BRACKET 
    SHARP m_id = METHOD_NAME
    nodes =list(node) r=regular_edges t=taken_edges e=exception_edges R_BRACKET {
        (M_id(m_id),{
            nodes = nodes;
            regular_edges= r;
            taken_edges = t;
            exception_edges=e
        })
    }

node: 
    |n_id = NODE L_SQR_BRACKET shape COMA l=label R_SQR_BRACKET SEMI_COLON {
        (N_id(n_id),l)
    }

shape:
    |SHAPE EQUAL RECORD {}

label: 
    |LABEL EQUAL QUOTE L_BRACKET l=list(instruction) QUOTE {l}

instruction:
    |p=P i=INSTRUCTION {Instr_tools.catch p i}

regular_edges:
    |SUBGRAPH REGULAR_EDGES sg =subgraph {sg}

taken_edges:
    |SUBGRAPH TAKEN_EDGES sg = subgraph {sg}

exception_edges:
    |SUBGRAPH EXCEPTION_EDGES sg =subgraph {sg}

subgraph:
    |L_BRACKET  EDGE SUBGRAPH_INFO SEMI_COLON l=list(edges) R_BRACKET {l}

edges: 
    |n1=NODE p1=P ARROW n2=NODE p2=P SEMI_COLON {
        ({node=N_id(n1);pc=Pc(p1)},{node=N_id(n2);pc=Pc(p2)})
    }
    |n1=NODE ARROW n2=NODE p2=P SEMI_COLON {
        ({node=N_id(n1);pc=Pc(0)},{node=N_id(n2);pc=Pc(p2)})
    }
