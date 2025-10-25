(** Lexer Module Interface *)

(** Token type for JSON parsing *)
type token =
  | T_LBrace
  | T_RBrace
  | T_LBracket
  | T_RBracket
  | T_Colon
  | T_Comma
  | T_String of string
  | T_Number of float
  | T_Bool of bool
  | T_Null
  | T_EOF

type lexer_state = { input : string; position : int; length : int }
(** Lexer state type *)

val init_lexer : string -> lexer_state
(** Initialize lexer state *)

val tokenize : lexer_state -> token * lexer_state
(** Tokenize input and return next token with updated state *)
