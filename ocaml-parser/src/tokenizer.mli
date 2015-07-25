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
  | QuoteString of string
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
  | Symbol of string
  | Then

val tokenize : string -> token list
val print_token : token -> unit
val print_tokens : token list -> unit
val print_tokens_debug : token list -> token list
val from_char_list : symbol -> token list
val to_string : token -> string
