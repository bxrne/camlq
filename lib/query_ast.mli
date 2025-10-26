(** Query AST Module Interface *)

(** Query expression types for jq-like JSON querying *)
type query_expr =
  | QRoot  (** Root object (.) *)
  | QProperty of string * query_expr  (** Property access (.key) *)
  | QIndex of int * query_expr  (** Array index access (.[0]) *)
  | QIterate of query_expr  (** Array iteration (.[]) *)

val to_string : query_expr -> string
(** Convert query expression to string representation *)
