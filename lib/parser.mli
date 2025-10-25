(** Parser Module Interface *)

open Ast
open Lexer

(** Result type for parsing outcomes *)
type parse_result =
  | PR_Success of json 
  | PR_Error of string 

(** Initialize parser state (wrapper around lexer) *)
val init_parser : string -> lexer_state

(** Parse input string and return result *)
val parse : lexer_state -> parse_result