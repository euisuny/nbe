
(* The type of tokens. *)

type token = 
  | VAR of (int)
  | RPAREN
  | LPAREN
  | LET
  | LAM
  | EOF
  | COMMA
  | APP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Debruijn.tm)
