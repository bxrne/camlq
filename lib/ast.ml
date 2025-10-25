(** JSON AST module **)

type json =
  | JObject of (string * json) list
  | JArray of json list
  | JString of string
  | JNumber of float
  | JBool of bool
  | JNull 

(** Convert JSON AST to string representation **)
let rec to_string (j : json) : string =
  match j with
  | JObject obj ->
      let members = List.map (fun (k, v) -> Printf.sprintf "\"%s\": %s" k (to_string v)) obj in
      "{" ^ String.concat ", " members ^ "}"
  | JArray arr ->
      let elements = List.map to_string arr in 
      "[" ^ String.concat ", " elements ^ "]"
  | JString s -> Printf.sprintf "\"%s\"" s
  | JNumber n -> string_of_float n 
  | JBool b -> string_of_bool b 
  | JNull -> "null" 

(* Alias for JSON type *)
module T = struct
  type t = json
end

include T

(* Expose to_string function *)
let to_string = to_string


