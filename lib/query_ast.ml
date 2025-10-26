(** Query AST Module Implementation *)

(** Query expression types for jq-like JSON querying *)
type query_expr =
  | QRoot  (** Root object (.) *)
  | QProperty of string * query_expr  (** Property access (.key) *)
  | QIndex of int * query_expr  (** Array index access (.[0]) *)
  | QIterate of query_expr  (** Array iteration (.[]) *)

(** Convert query expression to string representation *)
let rec to_string expr =
  let s =
    match expr with
    | QRoot -> ""
    | QProperty (key, next) -> to_string next ^ "." ^ key
    | QIndex (idx, next) -> to_string next ^ Printf.sprintf "[%d]" idx
    | QIterate next -> to_string next ^ "[]"
  in
  if s = "" then "." else s
