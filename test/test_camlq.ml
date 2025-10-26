open Camlq.Ast
open Camlq.Parser

(* Test helper functions *)
let assert_parse_success json_input expected_output =
  let state = init_parser json_input in
  match parse state with
  | PR_Success ast ->
      let result = Camlq.Ast.to_string ast in
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
  let result = Camlq.Ast.to_string obj in

  if result = expected then Printf.printf "✓ PASS: AST to_string\n"
  else
    Printf.printf "✗ FAIL: AST to_string\nExpected: %s\nGot: %s\n" expected
      result

let test_query_parsing () =
  print_endline "\n=== Testing Query Parsing ===";

  (* Test basic property access *)
  let test_query query expected_str =
    match Camlq.Query_parser.parse_query query with
    | Some expr ->
        let result = Camlq.Query_ast.to_string expr in
        if result = expected_str then
          Printf.printf "✓ PASS: parse '%s' -> '%s'\n" query result
        else
          Printf.printf "✗ FAIL: parse '%s'\nExpected: %s\nGot: %s\n" query
            expected_str result
    | None ->
        Printf.printf "✗ FAIL: parse '%s' -> None (expected %s)\n" query
          expected_str
  in

  test_query ".name" "..name";
  test_query ".address.city" "..address.city";
  test_query ".courses[0]" "..courses[0]";
  test_query ".courses[]" "..courses[]";

  (* Test invalid queries *)
  let test_invalid_query query =
    match Camlq.Query_parser.parse_query query with
    | None -> Printf.printf "✓ PASS: invalid query '%s' rejected\n" query
    | Some _ -> Printf.printf "✗ FAIL: invalid query '%s' accepted\n" query
  in

  test_invalid_query "";
  test_invalid_query "name";
  test_invalid_query "@invalid"

let test_query_evaluation () =
  print_endline "\n=== Testing Query Evaluation ===";

  (* Create test JSON data *)
  let test_json =
    JObject
      [
        ("name", JString "John Doe");
        ("age", JNumber 30.);
        ( "courses",
          JArray [ JString "Math"; JString "Science"; JString "History" ] );
        ( "address",
          JObject
            [
              ("street", JString "123 Main St");
              ("city", JString "Anytown");
              ("zip", JString "12345");
            ] );
        ("active", JBool true);
      ]
  in

  let test_eval query expected_results =
    match Camlq.Query_parser.parse_query query with
    | None -> Printf.printf "✗ FAIL: Could not parse query '%s'\n" query
    | Some expr ->
        let results = Camlq.Query_eval.eval_query expr test_json in
        let result_strs = List.map Camlq.Ast.to_string results in
        let expected_strs = List.map Camlq.Ast.to_string expected_results in
        if result_strs = expected_strs then
          Printf.printf "✓ PASS: eval '%s' -> %d results\n" query
            (List.length results)
        else
          Printf.printf "✗ FAIL: eval '%s'\nExpected: %s\nGot: %s\n" query
            (String.concat "; " expected_strs)
            (String.concat "; " result_strs)
  in

  (* Test property access *)
  test_eval ".name" [ JString "John Doe" ];
  test_eval ".age" [ JNumber 30. ];
  test_eval ".active" [ JBool true ];

  (* Test nested property access *)
  test_eval ".address.city" [ JString "Anytown" ];
  test_eval ".address.zip" [ JString "12345" ];

  (* Test array indexing *)
  test_eval ".courses[0]" [ JString "Math" ];
  test_eval ".courses[1]" [ JString "Science" ];
  test_eval ".courses[2]" [ JString "History" ];

  (* Test array iteration *)
  test_eval ".courses[]"
    [ JString "Math"; JString "Science"; JString "History" ];

  (* Test non-existent properties *)
  test_eval ".nonexistent" [];

  (* Test invalid array access *)
  test_eval ".courses[10]" [];
  test_eval ".name[0]" []

(* Main test runner *)
let () =
  print_endline "Running Camlq Tests...\n";

  test_basic_values ();
  test_arrays ();
  test_objects ();
  test_complex_json ();
  test_error_cases ();
  test_ast_to_string ();
  test_query_parsing ();
  test_query_evaluation ();

  print_endline "\nTest suite completed."
