(** Lexer Module Implementation *)

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

(** Initialize lexer state *)
let init_lexer (input : string) : lexer_state =
  { input; position = 0; length = String.length input }

(** Helper functions *)
let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let skip_whitespace (state : lexer_state) : lexer_state =
  let rec skip pos =
    if pos >= state.length then pos
    else if is_whitespace state.input.[pos] then skip (pos + 1)
    else pos
  in
  { state with position = skip state.position }

let peek_char (state : lexer_state) : char option =
  if state.position >= state.length then None
  else Some state.input.[state.position]

let advance (state : lexer_state) : lexer_state =
  { state with position = state.position + 1 }

(** Tokenize input and return next token with updated state *)
let tokenize (state : lexer_state) : token * lexer_state =
  let state = skip_whitespace state in
  match peek_char state with
  | None -> (T_EOF, state)
  | Some '{' -> (T_LBrace, advance state)
  | Some '}' -> (T_RBrace, advance state)
  | Some '[' -> (T_LBracket, advance state)
  | Some ']' -> (T_RBracket, advance state)
  | Some ':' -> (T_Colon, advance state)
  | Some ',' -> (T_Comma, advance state)
  | Some 't' ->
      (* Parse true *)
      if
        state.position + 3 <= state.length
        && state.input.[state.position] = 't'
        && state.input.[state.position + 1] = 'r'
        && state.input.[state.position + 2] = 'u'
        && state.input.[state.position + 3] = 'e'
      then (T_Bool true, { state with position = state.position + 4 })
      else (T_EOF, { state with position = state.length })
  | Some 'f' ->
      (* Parse false *)
      if
        state.position + 4 <= state.length
        && state.input.[state.position] = 'f'
        && state.input.[state.position + 1] = 'a'
        && state.input.[state.position + 2] = 'l'
        && state.input.[state.position + 3] = 's'
        && state.input.[state.position + 4] = 'e'
      then (T_Bool false, { state with position = state.position + 5 })
      else (T_EOF, { state with position = state.length })
  | Some 'n' ->
      (* Parse null *)
      if
        state.position + 3 <= state.length
        && state.input.[state.position] = 'n'
        && state.input.[state.position + 1] = 'u'
        && state.input.[state.position + 2] = 'l'
        && state.input.[state.position + 3] = 'l'
      then (T_Null, { state with position = state.position + 4 })
      else (T_EOF, { state with position = state.length })
  | Some '"' ->
      (* Parse string *)
      let rec parse_string pos =
        if pos >= state.length then state.length + 1
        else if state.input.[pos] = '"' then pos + 1
        else if state.input.[pos] = '\\' then
          if pos + 1 < state.length then parse_string (pos + 2)
          else state.length + 1
        else parse_string (pos + 1)
      in
      let end_pos = parse_string (state.position + 1) in
      if end_pos > state.length then
        (T_EOF, { state with position = state.length })
      else if end_pos = state.position + 1 then
        (* Empty string *)
        (T_String "", { state with position = end_pos })
      else
        let str_val =
          String.sub state.input (state.position + 1)
            (end_pos - state.position - 2)
        in
        (T_String str_val, { state with position = end_pos })
  | Some c when ('0' <= c && c <= '9') || c = '-' ->
      (* Parse number *)
      let rec parse_number pos =
        if pos >= state.length then pos
        else
          let c = state.input.[pos] in
          if
            ('0' <= c && c <= '9')
            || c = '.' || c = 'e' || c = 'E' || c = '-' || c = '+'
          then parse_number (pos + 1)
          else pos
      in
      let end_pos = parse_number state.position in
      let num_str =
        String.sub state.input state.position (end_pos - state.position)
      in
      let num_val = float_of_string num_str in
      (T_Number num_val, { state with position = end_pos })
  | Some _ -> (T_EOF, { state with position = state.length })
