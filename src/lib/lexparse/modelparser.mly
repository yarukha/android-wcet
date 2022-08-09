%{
    open Cfg_analysis
    open Power_model
%}
%token INVOKE REGISTERS
%token <string> METHOD
%token <float> VALUE
%token EOF

%start model 
%type <p_m option> model
%%


model: 
    |l = list(line) EOF {Some(List.fold_left (fun m (m',e,t)->M_s.add m' e (snd(m)),M_s.add m' t (fst(m))) (empty_m,empty_m) l)}
    |EOF {None}

line:
    |INVOKE REGISTERS m=METHOD e=VALUE t=VALUE {(m,e,t)}