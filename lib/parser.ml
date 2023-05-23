
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 10 "parser.mly"
       (int)
# 15 "parser.ml"
  )
    | RPAREN
    | LPAREN
    | LET
    | LAM
    | EOF
    | COMMA
    | APP
  
end

include MenhirBasics

# 1 "parser.mly"
  

open Debruijn



# 36 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_parse) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: parse. *)

  | MenhirState03 : (('s, _menhir_box_parse) _menhir_cell1_LET, _menhir_box_parse) _menhir_state
    (** State 03.
        Stack shape : LET.
        Start symbol: parse. *)

  | MenhirState05 : (('s, _menhir_box_parse) _menhir_cell1_LAM, _menhir_box_parse) _menhir_state
    (** State 05.
        Stack shape : LAM.
        Start symbol: parse. *)

  | MenhirState07 : (('s, _menhir_box_parse) _menhir_cell1_APP, _menhir_box_parse) _menhir_state
    (** State 07.
        Stack shape : APP.
        Start symbol: parse. *)

  | MenhirState09 : ((('s, _menhir_box_parse) _menhir_cell1_APP, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 09.
        Stack shape : APP expr.
        Start symbol: parse. *)

  | MenhirState15 : ((('s, _menhir_box_parse) _menhir_cell1_LET, _menhir_box_parse) _menhir_cell1_expr, _menhir_box_parse) _menhir_state
    (** State 15.
        Stack shape : LET expr.
        Start symbol: parse. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (
# 19 "parser.mly"
      (Debruijn.tm)
# 74 "parser.ml"
)

and ('s, 'r) _menhir_cell1_APP = 
  | MenhirCell1_APP of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LAM = 
  | MenhirCell1_LAM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and _menhir_box_parse = 
  | MenhirBox_parse of (
# 18 "parser.mly"
      (Debruijn.tm)
# 90 "parser.ml"
) [@@unboxed]

let _menhir_action_1 =
  fun _1 ->
    (
# 28 "parser.mly"
        ( Var(_1) )
# 98 "parser.ml"
     : (
# 19 "parser.mly"
      (Debruijn.tm)
# 102 "parser.ml"
    ))

let _menhir_action_2 =
  fun _3 _5 ->
    (
# 30 "parser.mly"
      ( App (_3, _5) )
# 110 "parser.ml"
     : (
# 19 "parser.mly"
      (Debruijn.tm)
# 114 "parser.ml"
    ))

let _menhir_action_3 =
  fun _3 ->
    (
# 31 "parser.mly"
                           ( Lam(_3) )
# 122 "parser.ml"
     : (
# 19 "parser.mly"
      (Debruijn.tm)
# 126 "parser.ml"
    ))

let _menhir_action_4 =
  fun _3 _5 ->
    (
# 33 "parser.mly"
      ( Let (_3, _5) )
# 134 "parser.ml"
     : (
# 19 "parser.mly"
      (Debruijn.tm)
# 138 "parser.ml"
    ))

let _menhir_action_5 =
  fun _1 ->
    (
# 24 "parser.mly"
             ( _1 )
# 146 "parser.ml"
     : (
# 18 "parser.mly"
      (Debruijn.tm)
# 150 "parser.ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | APP ->
        "APP"
    | COMMA ->
        "COMMA"
    | EOF ->
        "EOF"
    | LAM ->
        "LAM"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | RPAREN ->
        "RPAREN"
    | VAR _ ->
        "VAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_5 _1 in
          MenhirBox_parse _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v in
              let _v = _menhir_action_1 _1 in
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
          | LET ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
          | LAM ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
          | APP ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_1 _1 in
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LET ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | LAM ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | APP ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_LET, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_4 _3 _5 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_19 _menhir_stack _v _tok
      | MenhirState15 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState07 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_parse) _menhir_cell1_LAM -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LAM (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_3 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_APP, _menhir_box_parse) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_APP (_menhir_stack, _menhir_s) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_2 _3 _5 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. ((ttv_stack, _menhir_box_parse) _menhir_cell1_APP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parse) _menhir_state -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_1 _1 in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LET ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | LAM ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | APP ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LAM (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v in
              let _v = _menhir_action_1 _1 in
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LET ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
          | LAM ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
          | APP ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse) _menhir_state -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_APP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v in
              let _v = _menhir_action_1 _1 in
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07 _tok
          | LET ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
          | LAM ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
          | APP ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_1 _1 in
          _menhir_run_19 _menhir_stack _v _tok
      | LET ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LAM ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | APP ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | _ ->
          _eRR ()
  
end

let parse =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parse v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 36 "parser.mly"
  


# 394 "parser.ml"
