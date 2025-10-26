(** Parser Module Implementation *)

open Ast
open Lexer

(** Result type for parsing outcomes *)
type parse_result = PR_Success of json | PR_Error of string

(** Initialize parser state (wrapper around lexer) *)
let init_parser = init_lexer

(** Helper functions for character peeking (needed for object/array parsing) *)
let peek_char state =
  if state.position >= state.length then None
  else Some state.input.[state.position]

let advance state = { state with position = state.position + 1 }

(** Helper function to skip whitespace without consuming tokens *)
let skip_whitespace state =
  let rec skip pos =
    if pos >= state.length then pos
    else
      let c = state.input.[pos] in
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then skip (pos + 1)
      else pos
  in
  { state with position = skip state.position }

(** Parsing functions using continuation-passing style for tail recursion *)
let rec parse_value (state : lexer_state) (k : json * lexer_state -> 'a) : 'a =
  let state = skip_whitespace state in
  let token, new_state = tokenize state in
  match token with
  | T_String s -> k (JString s, new_state)
  | T_Number n -> k (JNumber n, new_state)
  | T_Bool b -> k (JBool b, new_state)
  | T_Null -> k (JNull, new_state)
  | T_LBrace -> parse_object new_state k
  | T_LBracket -> parse_array new_state k
  | T_EOF -> failwith "Unexpected EOF in parse_value"
  | _ -> failwith "Unexpected token in parse_value"

and parse_object (state : lexer_state) (k : json * lexer_state -> 'a) : 'a =
  let rec parse_pairs state acc k_inner =
    let state = skip_whitespace state in
    match peek_char state with
    | Some '}' -> k_inner (List.rev acc, advance state)
    | None -> failwith "Unexpected EOF in object"
    | _ ->
        let key, state1 = tokenize state in
        let state2 = skip_whitespace state1 in
        let colon_token, state3 = tokenize state2 in
        (match colon_token with
        | T_Colon -> ()
        | _ -> failwith "Expected colon in object");
        let state4 = skip_whitespace state3 in
        parse_value state4 (fun (value, state5) ->
          let new_acc =
            (match key with
            | T_String k -> (k, value)
            | _ -> failwith "Expected string key")
            :: acc
          in
          let state6 = skip_whitespace state5 in
          match peek_char state6 with
          | Some ',' -> parse_pairs (advance state6) new_acc k_inner
          | _ -> parse_pairs state6 new_acc k_inner
        )
  in
  parse_pairs state [] (fun (pairs, final_state) -> k (JObject pairs, final_state))

and parse_array (state : lexer_state) (k : json * lexer_state -> 'a) : 'a =
  let rec parse_elements state acc k_inner =
    let state = skip_whitespace state in
    match peek_char state with
    | Some ']' -> k_inner (List.rev acc, advance state)
    | None -> failwith "Unexpected EOF in array"
    | _ ->
        parse_value state (fun (value, state1) ->
          let new_acc = value :: acc in
          let state2 = skip_whitespace state1 in
          match peek_char state2 with
          | Some ',' -> parse_elements (advance state2) new_acc k_inner
          | _ -> parse_elements state2 new_acc k_inner
        )
  in
  parse_elements state [] (fun (elements, final_state) -> k (JArray elements, final_state))

(** Parse input string and return result *)
let parse (state : lexer_state) : parse_result =
  try
    parse_value state (fun (ast, final_state) ->
      let final_state = skip_whitespace final_state in
      match peek_char final_state with
      | None -> PR_Success ast
      | Some _ -> PR_Error "Extra input after parsing"
    )
  with
  | Failure msg -> PR_Error ("Parse error: " ^ msg)
  | exn -> PR_Error ("Unexpected error: " ^ Printexc.to_string exn)
