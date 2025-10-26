(** Query Parser Module Implementation *)

open Query_ast

(** Parse a property access segment (e.g., "key") *)
let parse_property query pos =
  if pos >= String.length query then None
  else if query.[pos] <> '.' then None
  else
    let start_pos = pos in
    (* Start from the dot position *)
    let rec find_end current_pos =
      if current_pos >= String.length query then current_pos
      else
        let c = query.[current_pos] in
        if c = '.' || c = '[' then current_pos else find_end (current_pos + 1)
    in
    let end_pos = find_end (start_pos + 1) in
    (* Start searching after the dot *)
    if start_pos + 1 = end_pos then None
    else
      let prop_name =
        String.sub query (start_pos + 1) (end_pos - start_pos - 1)
      in
      Some prop_name

(** Parse an index access segment (e.g., "[0]") *)
let parse_index query pos =
  if pos >= String.length query then None
  else if query.[pos] <> '[' then None
  else
    let start_pos = pos + 1 in
    let rec find_end current_pos =
      if current_pos >= String.length query then None
      else if query.[current_pos] = ']' then Some current_pos
      else find_end (current_pos + 1)
    in
    match find_end start_pos with
    | None -> None
    | Some end_pos -> (
        let index_str = String.sub query start_pos (end_pos - start_pos) in
        try
          let index = int_of_string index_str in
          Some (index, end_pos + 1)
        with Failure _ -> None)

(** Parse an iteration segment (e.g., "[]") *)
let parse_iterate query pos =
  if pos + 1 >= String.length query then None
  else if query.[pos] <> '[' || query.[pos + 1] <> ']' then None
  else Some (pos + 2)

(** Parse a complete query string into a query expression AST *)
let parse_query (query : string) : query_expr option =
  let rec parse_expr pos current_expr =
    if pos >= String.length query then Some current_expr
    else if query.[pos] = '.' then
      match parse_property query pos with
      | Some prop_name ->
          let next_pos = pos + 1 + String.length prop_name in
          let prop_expr = QProperty (prop_name, current_expr) in
          parse_expr next_pos prop_expr
      | None -> None
    else if query.[pos] = '[' then
      match parse_index query pos with
      | Some (index, next_pos) ->
          let index_expr = QIndex (index, current_expr) in
          parse_expr next_pos index_expr
      | None -> (
          match parse_iterate query pos with
          | Some next_pos ->
              let iter_expr = QIterate current_expr in
              parse_expr next_pos iter_expr
          | None -> None)
    else Some current_expr (* No more selectors *)
  in
  if String.length query = 0 || query.[0] <> '.' then None
  else parse_expr 0 QRoot
