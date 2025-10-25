open Camlq.Ast
open Camlq.Parser

let () =
  let test_input = "\"unclosed string" in
  Printf.printf "Testing: %s\n" test_input;
  let state = init_parser test_input in
  match parse state with
  | PR_Success ast -> 
      Printf.printf "SUCCESS: %s\n" (to_string ast)
  | PR_Error msg -> 
      Printf.printf "ERROR: %s\n" msg