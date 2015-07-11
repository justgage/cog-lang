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

val tokenize : string -> token list
val print_token : token -> unit
val print_tokens : token list -> unit
val from_str_list : string list -> token list

