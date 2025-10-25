open Camlq.Ast
open Camlq.Parser

let () =
  let test_json = "\"hello\"" in
  Printf.printf "Testing: %s\n" test_json;
  let state = init_parser test_json in
  match parse state with
  | PR_Success ast ->
      Printf.printf "Success: %s\n" (to_string ast)
  | PR_Error msg ->
      Printf.printf "Error: %s\n" msg