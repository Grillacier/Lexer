type def = string * automate
and
automate =
  |
  | Plus of expression * expression

let before =

let as_int = function
  | Int x -> int_of_string x

let test_input_symbols = function
  | Input_symbols i -> match i with:


type expression =
  | Var of string
  |
  | Or of expression * expression
  | And of expression * expression
  | In of expression * expression

let rec as_string = function
  | Var x -> x
  | True -> "true"
  | False -> "false"
  | Or (l, r) -> apply "\\/" l r
  | And (l, r) -> apply "/\\" l r
  | In (l, r) -> apply "in" l r

and apply op l r =
  "(" ^ as_string l ^ ") " ^ op ^ " (" ^ as_string r ^ ")"