type l (* for lists *)

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

val tokenize : string -> token list
val print_token : token -> unit
val print_tokens : token list -> unit
val from_str_list : string list -> token list

