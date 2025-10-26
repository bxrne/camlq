(** Query Parser Module Interface *)

open Query_ast

val parse_query : string -> query_expr option
(** Parse a query string into a query expression AST *)

val parse_property : string -> int -> string option
(** Parse a property access segment (e.g., "key") *)

val parse_index : string -> int -> (int * int) option
(** Parse an index access segment (e.g., "[0]") *)
