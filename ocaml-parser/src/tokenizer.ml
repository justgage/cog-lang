module Tokenizer = struct
  (** look in the .mli file for docs *)
  open Core.Std
  open Printf
  open Colors
  type l = string

  type symbol = char list

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
    | Equal
    | If
    | LessThan
    | LessThanOrEqual
    | LogicAnd
    | LogicOr
    | LogicNot
    | Minus
    | Newline
    | OpenRound
    | OpenSquare
    | Plus
    | Repeat
    | Display (* DEPRECATED *)
    | Until
    | Slash
    | Star
    | Symbol of string
    | Comma
    | Then
    | EndOfStatement
    | Arrow

  let to_string x =
    match x with
    | Arrow -> "->"
    | Boolean True -> "true"
    | Boolean False -> "false"
    | If -> "if"
    | Else -> "else"
    | LogicOr -> "or"
    | LogicAnd -> "and"
    | LogicNot -> "not"
    | Plus -> "+"
    | Minus -> "-"
    | Slash-> "/"
    | Star-> "*"
    | Repeat -> "repeat"
    | Display -> "display"
    | Until -> "until"
    | Box -> "box"
    | Assignment -> "="
    | DoubleQuote -> "\""
    | End -> "end"
    | Float x -> Printf.sprintf "%f" x
    | FuncDef -> "func"
    | GreaterThan -> ">"
    | GreaterThanOrEqual -> ">="
    | Equal -> "=="
    | LessThan -> "<"
    | LessThanOrEqual -> "<="
    | OpenRound -> "("
    | ClosingRound -> ")"
    | OpenSquare -> "["
    | ClosingSquare -> "]"
    | Newline -> "\n"
    | CommentBegin -> "#"
    | Comment x -> "#" ^ x
    | QuoteString x -> "\"" ^ x ^ "\""
    | Comma -> ","
    | Symbol x -> "Symbol=" ^ x
    | Then -> "then"
    | EndOfStatement -> ";"


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
    | "->" -> Arrow
    | "true" -> Boolean True
    | "false" -> Boolean False
    | "if" -> If
    | "else" -> Else
    | "box" -> Box
    | "or" -> LogicOr
    | "and" -> LogicAnd
    | "not" -> LogicNot
    | "=" -> Assignment
    | "\"" -> DoubleQuote
    | "end" -> End (* end tags *)
    | "func" -> FuncDef
    | "+" -> Plus
    | "-" -> Minus
    | "/" -> Slash
    | "*" -> Star
    | ">" -> GreaterThan
    | "==" -> Equal
    | ">=" -> GreaterThanOrEqual
    | "<" -> LessThan
    | "<=" -> LessThanOrEqual
    | "(" -> OpenRound
    | ")" -> ClosingRound
    | "[" -> OpenSquare
    | "]" -> ClosingSquare
    | "\n" -> Newline
    | "then" -> Then
    | "#" -> CommentBegin
    | "repeat" -> Repeat
    | "until" -> Until
    | "," -> Comma
    | ";" -> EndOfStatement
    | x when str_is_float x ->
        Float (Float.of_string x)
    | x -> Symbol x


  type operator_cat =
    | InfixOperator
    | PrefixOperator
    | Value
    | OtherSyntax
    | Seperator

  let operator_type token =
    match token with
    | ( Plus
      | Star
      | Slash
      | Assignment
      | Minus
      | LogicOr
      | LogicAnd
      | GreaterThan
      | GreaterThanOrEqual
      | Equal
      | LessThan
      | LessThanOrEqual
      | EndOfStatement
      | OpenRound
        ) -> InfixOperator
    | ( Boolean _
      | Float _
      | QuoteString _
      | Symbol _
      | Display
      ) -> Value
    | LogicNot -> PrefixOperator
    | Comma -> Seperator
    | ( Box
      | ClosingRound
      | ClosingSquare
      | CommentBegin
      | Comment _
      | DoubleQuote
      | Else
      | End
      | FuncDef
      | If
      | Repeat
      | Until
      | Newline
      | OpenSquare
      | Then
      | Arrow) -> OtherSyntax


  (* Butterfly operator? (this is my first operator in OCaml so I must
   * give it a name, obviously)
   *
   * This is an operator that will take a string and append it to
   * a char list
   * *)
  let (><) (left: string) (right : char list) =
    List.append (String.to_list left) right


  (** This will mostly put spaces around things that need it *)
  let rec spacer str_list = match str_list with
    | ',' ::rest -> " , " >< spacer rest
    | '"' ::rest -> " \" " >< spacer rest
    | '(' ::rest -> " ( "  >< spacer rest
    | ')' ::rest -> " ) "  >< spacer rest
    | '[' ::rest -> " [ "  >< spacer rest
    | ']' ::rest -> " ] "  >< spacer rest
    | '+' ::rest -> " + "  >< spacer rest
    (* v--- this breaks the arrow *)
    (* | '-' ::rest -> " - "  >< spacer rest *)
    | '/' ::rest -> " / "  >< spacer rest
    | '*' ::rest -> " * "  >< spacer rest
    | '\n'::rest -> " \n " >< spacer rest
    | '#'::rest  -> " # "  >< spacer rest
    | ';'::rest  -> " ; "  >< spacer rest
    | []         -> []
    | x::rest    -> x :: spacer rest


  (* An easy way to see how the tokenizer works *)
  let to_string_debug t = match t with
  | Symbol x ->
      Printf.sprintf "%s" x
  | (FuncDef | Box | If | Else | End | Repeat | Until) as x ->
      Printf.sprintf "%s" (Colors.cyan @@ to_string x)
  | (Assignment | Plus | Minus | Star) as x ->
      Printf.sprintf "%s" (Colors.yellow @@ to_string x)
  | (GreaterThan | GreaterThanOrEqual | LessThan | LessThanOrEqual) as x ->
      Printf.sprintf "%s" (Colors.yellow @@ to_string x)
  | Float x ->
      Printf.sprintf "%s" (Colors.green @@ Float.to_string x)
  | Newline ->
      Printf.sprintf "%s" (Colors.blue "⏎\n")
  | Comment x ->
      Printf.sprintf "%s" (Colors.magenta ("#" ^ x))
  | x -> (* tokens I just haven't spesified a color for *)
      Printf.sprintf "%s" (":" ^ (Colors.red @@ to_string  x) )

  let print_token t = Printf.printf "%s" (to_string_debug t)

  (* This will split the string by the first split_char, if not found,
   * everything is placed in the first return of the tuple *)
  let split_on_first split_char char_list =
    let i_opt = List.findi ~f:(fun _ x -> x = split_char) char_list in
    match i_opt with
    | Some (i,_) -> let (x, xs) = List.split_n char_list i in
                    (x, (List.drop xs 1))
    | None       -> (char_list, [])


  (*** Special tokens ***)

  (* This will grab a comment *)
  let comment_grab str =
    let (comment, rest) = split_on_first '\n' str in
    (Comment (String.of_char_list comment), rest)

  (* This will grab a string *)
  let string_grab str =
    let (str, rest) = split_on_first '"' str in
    (QuoteString (String.of_char_list str), rest)

  (* grabs everything from the head to the next space *)
  let next_str = split_on_first ' '

  (* takes a string and turns in into a string of tokens *)
  let rec from_char_list (str : char list) =
    match (next_str str) with
    | ([], []) -> []
    | (next, rest) ->
        let ftoken = from_str @@ String.of_char_list next in
        match ftoken with
         | CommentBegin -> let (comment, after_comment) = comment_grab rest in
             (* comment :: Newline :: *) (from_char_list after_comment)
         | DoubleQuote -> let (str, after_comment) = string_grab rest in
             str :: (from_char_list after_comment)
         | (Symbol "" | Newline) ->  from_char_list rest (* ignore *)
         | x            -> x :: from_char_list rest

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
end
