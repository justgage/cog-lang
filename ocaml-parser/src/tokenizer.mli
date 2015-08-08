module Tokenizer : sig
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
    | Display
    | Until
    | Slash
    | Star
    | Symbol of string
    | Comma
    | Then

  val tokenize : string -> token list
  val print_token : token -> unit
  val print_tokens : token list -> unit
  val print_tokens_debug : token list -> token list
  val from_char_list : symbol -> token list
  val to_string : token -> string
end
