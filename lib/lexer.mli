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

(** Lexer state type *)
type lexer_state = {
  input : string;
  position : int; 
  length : int;
}

(** Initialize lexer state *)
val init_lexer : string -> lexer_state

(** Tokenize input and return next token with updated state *)
val tokenize : lexer_state -> token * lexer_state