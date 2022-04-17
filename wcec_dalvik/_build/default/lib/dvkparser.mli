
(* The type of tokens. *)

type token = 
  | METHOD_START of (string)
  | INSTR of (string)
  | EOF
  | CLASS_START of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Dvk.program option)
