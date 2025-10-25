open Camlq

let read_all_stdin () =
  let rec loop acc =
    try
      let line = input_line stdin in
      loop (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  loop ""

let () =
  let json_input = read_all_stdin () in
  
  if String.length json_input = 0 then
    (
      print_endline "Usage:";
      print_endline "  cat file.json | camlq";
      print_endline "  echo '{\"key\": \"value\"}' | camlq";
      print_endline "";
      print_endline "Read JSON from stdin and parse it.";
      exit 0
    );
  
  let parser_state = Parser.init_parser json_input in 
  match Parser.parse parser_state with 
  | Parser.PR_Success ast ->
      print_endline (Ast.to_string ast)
  | Parser.PR_Error msg -> 
      Printf.printf "Error: %s\n" msg;
      exit 1