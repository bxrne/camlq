(** Parser Module **)

open Ast

(* Type definitions *)
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

(* Result type for parsing outcomes *)
type parse_result =
  | PR_Success of json 
  | PR_Error of string 

(* Parser state type *)
type parser_state = {
  input : string;
  position : int; 
  length : int;
}

(* Function to initialize parser state *)
let init_parser (input : string) : parser_state =
  { input; position = 0; length = String.length input }

(* Helper functions *)
let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let skip_whitespace (state : parser_state) : parser_state =
  let rec skip pos =
    if pos >= state.length then pos
    else if is_whitespace state.input.[pos] then skip (pos + 1)
    else pos
  in
  { state with position = skip state.position }

let peek_char (state : parser_state) : char option =
  if state.position >= state.length then None
  else Some state.input.[state.position]

let advance (state : parser_state) : parser_state =
  { state with position = state.position + 1 }

(* Simple tokenizer - converts string to tokens *)
let tokenize (state : parser_state) : (token * parser_state) =
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
      if state.position + 3 <= state.length &&
         state.input.[state.position] = 't' &&
         state.input.[state.position + 1] = 'r' &&
         state.input.[state.position + 2] = 'u' &&
         state.input.[state.position + 3] = 'e' then
        (T_Bool true, { state with position = state.position + 4 })
      else
        (T_EOF, { state with position = state.length })
  | Some 'f' ->
      (* Parse false *)
      if state.position + 4 <= state.length &&
         state.input.[state.position] = 'f' &&
         state.input.[state.position + 1] = 'a' &&
         state.input.[state.position + 2] = 'l' &&
         state.input.[state.position + 3] = 's' &&
         state.input.[state.position + 4] = 'e' then
        (T_Bool false, { state with position = state.position + 5 })
      else
        (T_EOF, { state with position = state.length })
  | Some 'n' ->
      (* Parse null *)
      if state.position + 3 <= state.length &&
         state.input.[state.position] = 'n' &&
         state.input.[state.position + 1] = 'u' &&
         state.input.[state.position + 2] = 'l' &&
         state.input.[state.position + 3] = 'l' then
        (T_Null, { state with position = state.position + 4 })
      else
        (T_EOF, { state with position = state.length })
  | Some '"' ->
      (* Parse string *)
      let rec parse_string pos =
        if pos >= state.length then pos
        else if state.input.[pos] = '"' then pos + 1
        else if state.input.[pos] = '\\' then 
          if pos + 1 < state.length then parse_string (pos + 2) else pos
        else parse_string (pos + 1)
      in
      let end_pos = parse_string (state.position + 1) in
      if end_pos > state.length then 
        (T_EOF, { state with position = state.length })
      else if end_pos = state.position + 1 then
        (* Empty string *)
        (T_String "", { state with position = end_pos })
      else
        let str_val = String.sub state.input (state.position + 1) (end_pos - state.position - 2) in
        (T_String str_val, { state with position = end_pos })
  | Some c when '0' <= c && c <= '9' || c = '-' ->
      (* Parse number *)
      let rec parse_number pos =
        if pos >= state.length then pos
        else
          let c = state.input.[pos] in
          if ('0' <= c && c <= '9') || c = '.' || c = 'e' || c = 'E' || c = '-' || c = '+' then
            parse_number (pos + 1)
          else pos
      in
      let end_pos = parse_number state.position in
      let num_str = String.sub state.input state.position (end_pos - state.position) in
      let num_val = float_of_string num_str in
      (T_Number num_val, { state with position = end_pos })
  | Some _ -> (T_EOF, { state with position = state.length })

(* Parsing functions *)
let rec parse_value (state : parser_state) : (json * parser_state) =
  let (token, new_state) = tokenize state in
  match token with
  | T_String s -> (JString s, new_state)
  | T_Number n -> (JNumber n, new_state)
  | T_Bool b -> (JBool b, new_state)
  | T_Null -> (JNull, new_state)
  | T_LBrace -> parse_object new_state
  | T_LBracket -> parse_array new_state
  | T_EOF -> failwith "Unexpected EOF in parse_value"
  | _ -> failwith ("Unexpected token in parse_value")

and parse_object (state : parser_state) : (json * parser_state) =
  let rec parse_pairs state acc =
    let state = skip_whitespace state in
    match peek_char state with
    | Some '}' -> (List.rev acc, advance state)
    | None -> failwith "Unexpected EOF in object"
    | _ ->
        let (key, state1) = tokenize state in
        let (colon_token, state2) = tokenize (skip_whitespace state1) in
        (match colon_token with
        | T_Colon -> ()
        | _ -> failwith "Expected colon in object");
        let (value, state3) = parse_value (skip_whitespace state2) in
        let new_acc = (match key with T_String k -> (k, value) | _ -> failwith "Expected string key") :: acc in
        let state4 = skip_whitespace state3 in
        match peek_char state4 with
        | Some ',' -> parse_pairs (advance state4) new_acc
        | _ -> parse_pairs state4 new_acc
  in
  let (pairs, final_state) = parse_pairs state [] in
  (JObject pairs, final_state)

and parse_array (state : parser_state) : (json * parser_state) =
  let rec parse_elements state acc =
    let state = skip_whitespace state in
    match peek_char state with
    | Some ']' -> (List.rev acc, advance state)
    | None -> failwith "Unexpected EOF in array"
    | _ ->
        let (value, state1) = parse_value state in
        let new_acc = value :: acc in
        let state2 = skip_whitespace state1 in
        match peek_char state2 with
        | Some ',' -> parse_elements (advance state2) new_acc
        | _ -> parse_elements state2 new_acc
  in
  let (elements, final_state) = parse_elements state [] in
  (JArray elements, final_state)

(* Function to parse input string *)
let parse (state : parser_state) : parse_result =
  try
    let (ast, final_state) = parse_value state in
    let final_state = skip_whitespace final_state in
    match peek_char final_state with
    | None -> PR_Success ast
    | Some _ -> PR_Error "Extra input after parsing"
  with
  | Failure msg -> PR_Error ("Parse error: " ^ msg)
  | exn -> PR_Error ("Unexpected error: " ^ Printexc.to_string exn)
