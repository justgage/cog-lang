open Core.Std
open Printf
open Colors
type l = string

type symbol = string

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

(**
 * These are simple meaningful or non-meaningful symbols
 * this step of the process does not structure it, it just
 * makes it more consumeable 
 **)
type token =
  | Boolean of boolean
  | If
  | Box
  | Assignment 
  | DoubleQuote
  | End (* end tags *)
  | Float of float
  | FuncDef
  | GreaterThan
  | GreaterThanOrEqual
  | LessThan
  | LessThanOrEqual
  | OpenRound
  | ClosingRound
  | OpenSquare
  | ClosingSquare
  | Symbol of symbol
  | Whitespace
  | Newline
  | CommentBegin

let to_string x =
  match x with
  | Boolean True -> "true" 
  | Boolean False -> "false" 
  | If -> "if" 
  | Box -> "box" 
  | Assignment -> "=" 
  | DoubleQuote -> "\""
  | End -> "end"
  | Float x -> Printf.sprintf "%f" x
  | FuncDef -> "func"
  | GreaterThan -> ">"
  | GreaterThanOrEqual -> ">="
  | LessThan -> "<"
  | LessThanOrEqual -> "<="
  | OpenRound -> "("
  | ClosingRound -> ")"
  | OpenSquare -> "["
  | ClosingSquare -> "]"
  | Whitespace -> ""
  | Newline -> "\n"
  | CommentBegin -> "#"
  | Symbol x -> x 

let from_str x =
  match x with
  | "true" -> Boolean True
  | "false" -> Boolean False
  | "if" -> If
  | "box" -> Box
  | "=" -> Assignment
  | "\""-> DoubleQuote
  | "end"-> End (* end tags *)
  | "1"-> Float 1.0 (* TODO: FIX ME <---------------*)
  | "func"-> FuncDef
  | ">"-> GreaterThan
  | ">="-> GreaterThanOrEqual
  | "<"-> LessThan
  | "<="-> LessThanOrEqual
  | "("-> OpenRound
  | ")"-> ClosingRound
  | "["-> OpenSquare
  | "]"-> ClosingSquare
  | ""-> Whitespace
  | "\n"-> Newline
  | "#"-> CommentBegin
  | x -> Symbol x

(** This will mostly put spaces around things that need it *)
let rec spacer str_list = match str_list with
  | '"' ::rest -> " \" " ^ spacer rest
  | '(' ::rest -> " ( " ^ spacer rest
  | ')' ::rest -> " ) " ^ spacer rest
  | '[' ::rest -> " [ " ^ spacer rest
  | ']' ::rest -> " ] " ^ spacer rest
  | '+' ::rest -> " + " ^ spacer rest
  | '-' ::rest -> " - " ^ spacer rest
  | '/' ::rest -> " / " ^ spacer rest
  | '*' ::rest -> " * " ^ spacer rest
  | '\n'::rest -> " \n " ^ spacer rest
  | '#'::rest  -> " # " ^ spacer rest
  | []         -> ""
  | x::rest    -> Char.to_string x ^ spacer rest

let print_token t = match t with
| Symbol x -> Printf.printf ":%s: " (Colors.cyan x)
| x -> Printf.printf "%s " (Colors.normal ^ (to_string x))

let split_up = String.split ~on:' '
let from_str_list = List.map ~f:from_str
let print_tokens tokens = List.iter ~f:(print_token) tokens; print_endline "" 

(* the high level function used to tokenize a string *)
let tokenize str = 
  str
  |> String.to_list
  |> spacer
  |> split_up
  |> from_str_list
