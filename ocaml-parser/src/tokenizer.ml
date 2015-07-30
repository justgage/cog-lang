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
    | QuoteString x -> "\"" ^ x ^ "\""
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
    | '"' ::rest -> " \" " >< spacer rest
    | '(' ::rest -> " ( "  >< spacer rest
    | ')' ::rest -> " ) "  >< spacer rest
    | '[' ::rest -> " [ "  >< spacer rest
    | ']' ::rest -> " ] "  >< spacer rest
    | '+' ::rest -> " + "  >< spacer rest
    | '-' ::rest -> " - "  >< spacer rest
    | '/' ::rest -> " / "  >< spacer rest
    | '*' ::rest -> " * "  >< spacer rest
    | '\n'::rest -> " \n " >< spacer rest
    | '#'::rest  -> " # "  >< spacer rest
    | []         -> []
    | x::rest    -> x :: spacer rest

  (* An easy way to see how the tokenizer works *)
  let print_token t = match t with
  | Symbol x ->
      Printf.printf "%s" x
  | (FuncDef | Box | If | Else | End | Repeat | RepeatTill) as x ->
      Printf.printf "%s" (Colors.cyan @@ to_string x)
  | (Assignment | Plus | Minus | Star) as x ->
      Printf.printf "%s" (Colors.yellow @@ to_string x)
  | (GreaterThan | GreaterThanOrEqual | LessThan | LessThanOrEqual) as x ->
      Printf.printf "%s" (Colors.yellow @@ to_string x)
  | Float x ->
      Printf.printf "%s" (Colors.green @@ Float.to_string x)
  | Newline -> 
      Printf.printf "%s" (Colors.blue "⏎\n")
  | Comment x ->
      Printf.printf "%s" (Colors.magenta ("#" ^ x))
  | x -> (* tokens I just haven't spesified a color for *)
      Printf.printf "%s" (Colors.red @@ to_string x)

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
             comment :: Newline :: (from_char_list after_comment)
         | DoubleQuote -> let (str, after_comment) = string_grab rest in
             str :: (from_char_list after_comment)
         | Symbol "" ->  (from_char_list rest)
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

end
