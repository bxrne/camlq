open Camlq
let sample_json = "
{
  \"name\": \"John Doe\",
  \"age\": 30,
  \"is_student\": false,
  \"courses\": [\"Math\", \"Science\", \"History\"],
  \"address\": {
    \"street\": \"123 Main St\",
    \"city\": \"Anytown\",
    \"zip\": \"12345\"
  }
}
"

let () =
  print_endline "Sample JSON Data:";
  print_endline sample_json;

  let parser_state = Parser.init_parser sample_json in 
  match Parser.parse parser_state with 
  | Parser.PR_Success ast ->
      print_endline "Parsing succeeded: ";
      print_endline (Ast.to_string ast)
  | Parser.PR_Error msg -> 
      Printf.printf "Parsing failed with error: %s\n" msg




