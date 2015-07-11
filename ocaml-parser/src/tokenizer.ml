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
  | Assignment
  | Boolean of boolean
  | Box
  | ClosingRound
  | ClosingSquare
  | CommentBegin
  | DoubleQuote
  | Else
  | End (* end tags *)
  | Float of float
  | FuncDef
  | GreaterThan
  | GreaterThanOrEqual
  | If
  | LessThan
  | LessThanOrEqual
  | LogicAnd
  | LogicOr
  | Minus
  | Newline
  | OpenRound
  | OpenSquare
  | Plus
  | Repeat
  | RepeatTill
  | Slash
  | Star
  | Symbol of symbol
  | Then

let to_string x =
  match x with
  | Boolean True -> "true" 
  | Boolean False -> "false" 
  | If -> "if" 
  | Else -> "else" 
  | LogicOr -> "or"
  | LogicAnd -> "and"
  | Plus -> "+" 
  | Minus -> "-"
  | Slash-> "/"
  | Star-> "*"
  | Repeat -> "repeat" 
  | RepeatTill -> "repeat_till" 
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
  | Then -> "then"
  | Newline -> "\n"
  | CommentBegin -> "#"
  | Symbol x -> x 

let str_is_float str =  
  let maybe_float = Option.try_with (
    fun () -> Float.of_string str
    )
  in
  match maybe_float with
  | Some _ -> true
  | None  -> false

let from_str x =
  match x with
  | "true" -> Boolean True
  | "false" -> Boolean False
  | "if" -> If
  | "else" -> Else
  | "box" -> Box
  | "or" -> LogicOr
  | "and" -> LogicAnd
  | "=" -> Assignment
  | "\"" -> DoubleQuote
  | "end" -> End (* end tags *)
  | "func" -> FuncDef
  | "+" -> Plus
  | " -" -> Minus
  | "/" -> Slash
  | "*" -> Star
  | ">" -> GreaterThan
  | ">=" -> GreaterThanOrEqual
  | "<" -> LessThan
  | "<=" -> LessThanOrEqual
  | "(" -> OpenRound
  | ")" -> ClosingRound
  | "[" -> OpenSquare
  | "]" -> ClosingSquare
  | ("\n" | ";" | "then") -> Newline
  | "#" -> CommentBegin
  | "repeat" -> Repeat
  | "repeat_till" -> RepeatTill
  | x when str_is_float x -> 
      Float (Float.of_string x)
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

(* An easy way to see how the tokenizer works *)
let print_token t = match t with
| Symbol x ->
    Printf.printf "%s " x
| (FuncDef | Box | If | Else | End | Repeat | RepeatTill) as x ->
    Printf.printf "%s " (Colors.cyan @@ to_string x)
| (Assignment | Plus | Minus | Star) as x ->
    Printf.printf "%s " (Colors.yellow @@ to_string x)
| (GreaterThan | GreaterThanOrEqual | LessThan | LessThanOrEqual) as x ->
    Printf.printf "%s " (Colors.yellow @@ to_string x)
| Float x ->
    Printf.printf "%s " (Colors.green @@ Float.to_string x)
| Newline -> 
    Printf.printf "%s" (Colors.blue "⏎\n")
| CommentBegin ->
    Printf.printf "%s " (Colors.magenta "#")
| x -> (* tokens I just haven't spesified a color for *)
    Printf.printf "%s " (Colors.magenta @@ to_string x)


let split_up = String.split ~on:' '
let from_str_list list = (List.map ~f:from_str list) @ [Newline]
let print_tokens tokens = List.iter ~f:(print_token) tokens

(* the high level function used to tokenize a string *)
let tokenize str = 
  str
  |> String.to_list
  |> spacer
  |> split_up
  |> from_str_list
