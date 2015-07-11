type symbol

type boolean =
  | True
  | False

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

