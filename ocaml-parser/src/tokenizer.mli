type symbol

type boolean =
  | True
  | False

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
  | Comment of string
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
val from_char_list : symbol -> token list
val to_string : token -> string
