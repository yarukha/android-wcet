
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | METHOD_START of (
# 6 "lib/dvkparser.mly"
       (string)
# 15 "lib/dvkparser.ml"
  )
    | INSTR of (
# 7 "lib/dvkparser.mly"
       (string)
# 20 "lib/dvkparser.ml"
  )
    | EOF
    | CLASS_START of (
# 5 "lib/dvkparser.mly"
       (string)
# 26 "lib/dvkparser.ml"
  )
  
end

include MenhirBasics

# 1 "lib/dvkparser.mly"
  
    open Dvk

# 37 "lib/dvkparser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_program) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: program. *)

  | MenhirState02 : (('s, _menhir_box_program) _menhir_cell1_CLASS_START, _menhir_box_program) _menhir_state
    (** State 02.
        Stack shape : CLASS_START.
        Start symbol: program. *)

  | MenhirState03 : (('s, _menhir_box_program) _menhir_cell1_METHOD_START, _menhir_box_program) _menhir_state
    (** State 03.
        Stack shape : METHOD_START.
        Start symbol: program. *)

  | MenhirState06 : (('s, _menhir_box_program) _menhir_cell1_instruction, _menhir_box_program) _menhir_state
    (** State 06.
        Stack shape : instruction.
        Start symbol: program. *)

  | MenhirState08 : (('s, _menhir_box_program) _menhir_cell1_methode, _menhir_box_program) _menhir_state
    (** State 08.
        Stack shape : methode.
        Start symbol: program. *)

  | MenhirState14 : (('s, _menhir_box_program) _menhir_cell1_classe, _menhir_box_program) _menhir_state
    (** State 14.
        Stack shape : classe.
        Start symbol: program. *)


and ('s, 'r) _menhir_cell1_classe = 
  | MenhirCell1_classe of 's * ('s, 'r) _menhir_state * (Dvk.classe)

and ('s, 'r) _menhir_cell1_instruction = 
  | MenhirCell1_instruction of 's * ('s, 'r) _menhir_state * (Dvk.instruction)

and ('s, 'r) _menhir_cell1_methode = 
  | MenhirCell1_methode of 's * ('s, 'r) _menhir_state * (Dvk.methode)

and ('s, 'r) _menhir_cell1_CLASS_START = 
  | MenhirCell1_CLASS_START of 's * ('s, 'r) _menhir_state * (
# 5 "lib/dvkparser.mly"
       (string)
# 84 "lib/dvkparser.ml"
)

and ('s, 'r) _menhir_cell1_METHOD_START = 
  | MenhirCell1_METHOD_START of 's * ('s, 'r) _menhir_state * (
# 6 "lib/dvkparser.mly"
       (string)
# 91 "lib/dvkparser.ml"
)

and _menhir_box_program = 
  | MenhirBox_program of (Dvk.program option) [@@unboxed]

let _menhir_action_01 =
  fun c id ->
    (
# 21 "lib/dvkparser.mly"
                                          ((id,c))
# 102 "lib/dvkparser.ml"
     : (Dvk.classe))

let _menhir_action_02 =
  fun instr ->
    (
# 29 "lib/dvkparser.mly"
                   (Inst(instr))
# 110 "lib/dvkparser.ml"
     : (Dvk.instruction))

let _menhir_action_03 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 118 "lib/dvkparser.ml"
     : (Dvk.instruction list))

let _menhir_action_04 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 126 "lib/dvkparser.ml"
     : (Dvk.instruction list))

let _menhir_action_05 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 134 "lib/dvkparser.ml"
     : (Dvk.methode list))

let _menhir_action_06 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 142 "lib/dvkparser.ml"
     : (Dvk.methode list))

let _menhir_action_07 =
  fun id instr ->
    (
# 26 "lib/dvkparser.mly"
                                                   ((id,instr))
# 150 "lib/dvkparser.ml"
     : (Dvk.methode))

let _menhir_action_08 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 158 "lib/dvkparser.ml"
     : (Dvk.program))

let _menhir_action_09 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 166 "lib/dvkparser.ml"
     : (Dvk.program))

let _menhir_action_10 =
  fun p ->
    (
# 16 "lib/dvkparser.mly"
                                   (Some(p))
# 174 "lib/dvkparser.ml"
     : (Dvk.program option))

let _menhir_action_11 =
  fun () ->
    (
# 17 "lib/dvkparser.mly"
         (None)
# 182 "lib/dvkparser.ml"
     : (Dvk.program option))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | CLASS_START _ ->
        "CLASS_START"
    | EOF ->
        "EOF"
    | INSTR _ ->
        "INSTR"
    | METHOD_START _ ->
        "METHOD_START"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_12 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let p = _v in
      let _v = _menhir_action_10 p in
      MenhirBox_program _v
  
  let rec _menhir_goto_nonempty_list_classe_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _v
      | MenhirState00 ->
          _menhir_run_12 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_classe -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let MenhirCell1_classe (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_09 x xs in
      _menhir_goto_nonempty_list_classe_ _menhir_stack _v _menhir_s
  
  let rec _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_CLASS_START (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | METHOD_START _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02
      | CLASS_START _ | EOF ->
          let _v = _menhir_action_05 () in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_METHOD_START (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INSTR _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let instr = _v_0 in
          let _v = _menhir_action_02 instr in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | CLASS_START _ | EOF | METHOD_START _ ->
          let _v = _menhir_action_03 () in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | INSTR _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let instr = _v_0 in
          let _v = _menhir_action_02 instr in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
      | CLASS_START _ | EOF | METHOD_START _ ->
          let _v = _menhir_action_03 () in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_07 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_instruction -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_instruction (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_04 x xs in
      _menhir_goto_list_instruction_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_instruction_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState06 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_05 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_METHOD_START -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_METHOD_START (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let instr = _v in
      let _v = _menhir_action_07 id instr in
      let _menhir_stack = MenhirCell1_methode (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | METHOD_START _v_0 ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState08
      | CLASS_START _ | EOF ->
          let _v = _menhir_action_05 () in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_methode -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_methode (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_06 x xs in
      _menhir_goto_list_methode_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_methode_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState02 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_CLASS_START -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CLASS_START (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let c = _v in
      let _v = _menhir_action_01 c id in
      match (_tok : MenhirBasics.token) with
      | CLASS_START _v_0 ->
          let _menhir_stack = MenhirCell1_classe (_menhir_stack, _menhir_s, _v) in
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState14
      | EOF ->
          let x = _v in
          let _v = _menhir_action_08 x in
          _menhir_goto_nonempty_list_classe_ _menhir_stack _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _v = _menhir_action_11 () in
          MenhirBox_program _v
      | CLASS_START _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00
      | _ ->
          _eRR ()
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
