open Core.Std
open Printf
type l = string

type symbol

type boolean =
  | True
  | False

type bolean_expression =
  | LessThan of (boolean * boolean)
  | GreaterThan of (boolean * boolean)
  | LessThanOrEqual of (boolean * boolean)
  | GreaterThanOrEqual of (boolean * boolean)

type user_func
type built_in


type function_exec = 
  | BuiltIn of built_in
  | UserFunc of user_func


type expression =
  | FunctionExec of function_exec
  | Adition of (expression * expression)
  | Division of (expression * expression)
  | Boolean of boolean


type statement =
  | Display
  | BoxDef of (symbol * expression)
  | BoxAssign
  | RepeatTill
  | Expression of expression


type function_def = {
  name : symbol;
  args : expression list;
  body : statement list;
}

type token =
  | String of string
  | Float of float
  | Expression of expression
  | Boolean of boolean
  | Whitespace of string
  | List of l
  | Statement of statement
  | End (* end tags *)

(* type ast = token : l *)


let print_token x =
  match x with
  | String _ ->  printf "string "
  | Float _ ->  printf "float "
  | Expression _ -> printf "expression "
  | Boolean _ ->  printf "boolean "
  | Whitespace _ ->  printf "whitespace "
  | List _ ->  printf "list "
  | Statement _ -> printf "statement "
  | End -> printf "end "

let from_str_list = List.map ~f:(fun _ -> Boolean True)

let print_tokens tokens = List.iter ~f:(print_token) tokens; print_endline "" 

