(** Parser Module Interface *)

open Ast
open Lexer

(** Result type for parsing outcomes *)
type parse_result = PR_Success of json | PR_Error of string

val init_parser : string -> lexer_state
(** Initialize parser state (wrapper around lexer) *)

val parse : lexer_state -> parse_result
(** Parse input string and return result *)
