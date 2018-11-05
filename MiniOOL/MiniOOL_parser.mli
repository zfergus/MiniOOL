
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TIMES
  | SKIP
  | SEMICOLON
  | RPAREN
  | RBRACE
  | PROC
  | PLUS
  | PARALLEL
  | OR
  | NUM of (int)
  | NULL
  | NOT
  | MINUS
  | MALLOC
  | LPAREN
  | LBRACE
  | IS_NOT_EQUAL
  | IS_LESS_EQUAL
  | IS_LESS
  | IS_GREATER_EQUAL
  | IS_GREATER
  | IS_EQUAL
  | IF
  | IDENT of (string)
  | EOL
  | ELSE
  | DOT
  | DIV
  | COLON
  | BOOL of (bool)
  | ATOM
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
