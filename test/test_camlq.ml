open Camlq.Ast
open Camlq.Parser

(* Test helper functions *)
let assert_parse_success json_input expected_output =
  let state = init_parser json_input in
  match parse state with
  | PR_Success ast ->
      let result = to_string ast in
      if result = expected_output then Printf.printf "✓ PASS: %s\n" json_input
      else
        Printf.printf "✗ FAIL: %s\nExpected: %s\nGot: %s\n" json_input
          expected_output result
  | PR_Error msg -> Printf.printf "✗ FAIL: %s\nError: %s\n" json_input msg

let assert_parse_error json_input expected_error =
  let state = init_parser json_input in
  match parse state with
  | PR_Success _ ->
      Printf.printf "✗ FAIL: %s\nExpected error but got success\n" json_input
  | PR_Error msg ->
      if String.contains msg expected_error.[0] then
        Printf.printf "✓ PASS: %s -> %s\n" json_input msg
      else
        Printf.printf "✗ FAIL: %s\nExpected error containing '%s', got: %s\n"
          json_input expected_error msg

(* Test cases *)
let test_basic_values () =
  print_endline "=== Testing Basic Values ===";

  (* Test strings *)
  assert_parse_success "\"hello\"" "\"hello\"";
  assert_parse_success "\"\"" "\"\"";

  (* Test numbers *)
  assert_parse_success "42" "42.";
  assert_parse_success "3.14" "3.14";
  assert_parse_success "-1" "-1.";

  (* Test booleans *)
  assert_parse_success "true" "true";
  assert_parse_success "false" "false";

  (* Test null *)
  assert_parse_success "null" "null"

let test_arrays () =
  print_endline "\n=== Testing Arrays ===";

  assert_parse_success "[]" "[]";
  assert_parse_success "[1,2,3]" "[1., 2., 3.]";
  assert_parse_success "[\"a\",\"b\",\"c\"]" "[\"a\", \"b\", \"c\"]";
  assert_parse_success "[true,false,null]" "[true, false, null]"

let test_objects () =
  print_endline "\n=== Testing Objects ===";

  assert_parse_success "{}" "{}";
  assert_parse_success "{\"key\":\"value\"}" "{\"key\": \"value\"}";
  assert_parse_success "{\"a\":1,\"b\":2}" "{\"a\": 1., \"b\": 2.}";
  assert_parse_success "{\"nested\":{\"inner\":42}}"
    "{\"nested\": {\"inner\": 42.}}"

let test_complex_json () =
  print_endline "\n=== Testing Complex JSON ===";

  let complex_json =
    "{\n\
    \    \"name\": \"John Doe\",\n\
    \    \"age\": 30,\n\
    \    \"is_student\": false,\n\
    \    \"courses\": [\"Math\", \"Science\", \"History\"],\n\
    \    \"address\": {\n\
    \      \"street\": \"123 Main St\",\n\
    \      \"city\": \"Anytown\",\n\
    \      \"zip\": \"12345\"\n\
    \    }\n\
    \  }"
  in

  let expected =
    "{\"name\": \"John Doe\", \"age\": 30., \"is_student\": false, \
     \"courses\": [\"Math\", \"Science\", \"History\"], \"address\": \
     {\"street\": \"123 Main St\", \"city\": \"Anytown\", \"zip\": \"12345\"}}"
  in

  assert_parse_success complex_json expected

let test_error_cases () =
  print_endline "\n=== Testing Error Cases ===";

  assert_parse_error "{" "Parse error";
  assert_parse_error "[" "Parse error";
  assert_parse_error "\"unclosed string" "Parse error";
  assert_parse_error "{invalid}" "Parse error";
  assert_parse_error "123abc" "Extra input"

let test_ast_to_string () =
  print_endline "\n=== Testing AST to_string ===";

  (* Test direct AST construction *)
  let obj =
    JObject
      [
        ("name", JString "Alice");
        ("age", JNumber 25.);
        ("active", JBool true);
        ("tags", JArray [ JString "dev"; JString "ocaml" ]);
      ]
  in

  let expected =
    "{\"name\": \"Alice\", \"age\": 25., \"active\": true, \"tags\": [\"dev\", \
     \"ocaml\"]}"
  in
  let result = to_string obj in

  if result = expected then Printf.printf "✓ PASS: AST to_string\n"
  else
    Printf.printf "✗ FAIL: AST to_string\nExpected: %s\nGot: %s\n" expected
      result

(* Main test runner *)
let () =
  print_endline "Running Camlq Tests...\n";

  test_basic_values ();
  test_arrays ();
  test_objects ();
  test_complex_json ();
  test_error_cases ();
  test_ast_to_string ();

  print_endline "\nTest suite completed."
