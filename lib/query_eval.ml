(** Query Evaluator Module Implementation *)

open Ast
open Query_ast

(** Evaluate property access on JSON object *)
let eval_property prop_name json =
  match json with
  | JObject pairs ->
      List.find_map (fun (k, v) -> if k = prop_name then Some v else None) pairs
  | _ -> None

(** Evaluate array index access on JSON array *)
let eval_index index json =
  match json with
  | JArray elements ->
      if index >= 0 && index < List.length elements then
        Some (List.nth elements index)
      else None
  | _ -> None

(** Evaluate array iteration, returning all elements *)
let eval_iterate json = match json with JArray elements -> elements | _ -> []

(** Evaluate a query expression on JSON data, returning a list of matching
    values *)
let rec eval_query expr json =
  match expr with
  | QRoot -> [ json ]
  | QProperty (prop, next_expr) ->
      let inner_results = eval_query next_expr json in
      List.concat_map
        (fun result ->
          match eval_property prop result with
          | Some prop_value -> [ prop_value ]
          | None -> [])
        inner_results
  | QIndex (idx, next_expr) ->
      let inner_results = eval_query next_expr json in
      List.concat_map
        (fun result ->
          match eval_index idx result with
          | Some indexed_value -> [ indexed_value ]
          | None -> [])
        inner_results
  | QIterate next_expr ->
      let inner_results = eval_query next_expr json in
      List.concat_map
        (fun result ->
          let elements = eval_iterate result in
          elements)
        inner_results
