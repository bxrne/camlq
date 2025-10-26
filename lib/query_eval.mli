(** Query Evaluator Module Interface *)

open Ast
open Query_ast

val eval_query : query_expr -> json -> json list
(** Evaluate a query expression on JSON data, returning a list of matching
    values *)

val eval_property : string -> json -> json option
(** Evaluate property access on JSON object *)

val eval_index : int -> json -> json option
(** Evaluate array index access on JSON array *)

val eval_iterate : json -> json list
(** Evaluate array iteration, returning all elements *)
