(** look in the .mli file for docs *)
open Core.Std
open Printf
open Colors
type l = string

type symbol = string

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
  | Comment x -> "#" ^ x
  | Symbol x -> "Symbol=" ^ x 

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
| Comment x ->
    Printf.printf "%s " (Colors.magenta ("#" ^ x))
| x -> (* tokens I just haven't spesified a color for *)
    Printf.printf "%s " (Colors.red @@ to_string x)

let split_1 str index = 
  (
    String.slice str 0 index , 
    String.slice str (index + 1) (String.length str) 
    (*                      ^-  that this takes out the space *)
  )

(* grabs everything from the head to the next space *)
let next_str str = match String.index str ' ' with
| Some index -> Some (split_1 str index)
| None -> None


(* This will split the string by the first split_char, if not found,
 * everything is placed in the first return of the tuple *)
let split_first split_char str =
  let i_opt = List.findi ~f:(fun _ x -> x = split_char) str in
  match i_opt with
  | Some (i,_) -> List.split_n str i 
  | None       -> (str, [])

let comment_grab str =
  let (comment, rest) = split_first '\n' str in
  (Comment (String.of_char_list comment), rest)

let comment_grab_str str =
  ( comment_grab @@ String.to_list str)

(* takes a string and turns in into a string of tokens *)
let rec from_char_list str = 
  match (next_str str) with
  | None -> []
  | Some (next, rest) ->
      let ftoken = from_str next in
      match ftoken with
       | CommentBegin -> let (comment, after_comment) = comment_grab_str rest in
           comment :: (from_char_list (String.of_char_list after_comment))
       | x            -> x :: (from_char_list rest)

let print_tokens tokens = 
 tokens |> List.iter ~f:(print_token) 

let print_tokens_debug tokens = 
 tokens |> List.map ~f:(fun x -> print_token x; x) 

(* the high level function used to tokenize a string *)
let tokenize str = 
  str
  |> String.to_list
  |> spacer
  |> from_char_list

