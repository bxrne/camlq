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
  let args = Sys.argv in
  let query_arg = if Array.length args > 1 then Some args.(1) else None in

  let json_input = read_all_stdin () in

  if String.length json_input = 0 then (
    print_endline "Usage:";
    print_endline "  cat file.json | camlq <query>";
    print_endline " query: optional jq-like query to filter JSON data";
    print_endline "";
    print_endline "  echo '{\"key\": \"value\"}' | camlq";
    print_endline "";
    print_endline "Read JSON from stdin and parse it.";
    exit 0);

  let parser_state = Parser.init_parser json_input in
  match Parser.parse parser_state with
  | Parser.PR_Error msg ->
      Printf.printf "Error: %s\n" msg;
      exit 1
  | Parser.PR_Success ast -> (
      match query_arg with
      | None -> print_endline (Ast.to_string ast)
      | Some query_str -> (
          match Query_parser.parse_query query_str with
          | None ->
              Printf.printf "Error: Invalid query '%s'\n" query_str;
              exit 1
          | Some query_expr ->
              let results = Query_eval.eval_query query_expr ast in
              if results = [] then (
                Printf.printf "Error: Query '%s' returned no results\n"
                  query_str;
                exit 1)
              else
                List.iter
                  (fun result -> print_endline (Ast.to_string result))
                  results))
