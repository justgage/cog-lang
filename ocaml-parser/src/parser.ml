module Parser = struct
  open Core.Std
  open Tokenizer


  type symbol
  type user_func = {
    name : string
  }
  type keyword

  type boolean =
    | True
    | False

  type boolean_expression =
    | LessThan of (boolean * boolean)
    | GreaterThan of (boolean * boolean)
    | LessThanOrEqual of (boolean * boolean)
    | GreaterThanOrEqual of (boolean * boolean)

  (**
   * This represents a function call
   * *)
  type function_exec = 
    | Keyword of keyword
    | UserFunc of user_func


  type expression =
    | FunctionExec of function_exec
    | Addition of (expression * expression)
    | Division of (expression * expression)
    | Boolean of boolean_expression
    | StringEx of string
    | Float of float

  type box_assign = {
    var_name : string;
    expression : expression;
  }

  type statement  =
    | Display of string
    | BoxDef of (symbol * expression)
    | BoxAssign of box_assign
    | RepeatTill
    | Expression of expression
    | BadToken of int * string 

  type function_def = {
    name : symbol;
    args : expression list;
    body : expression list;
  }

  type ast = statement list

  (* gets the things till the closing paren *)
  let rec get_args args = match args with
  | Tokenizer.ClosingRound::rest -> 
      []
  | x::rest -> 
      x :: (get_args rest)
  | [] -> 
      failwith "Unexpected end of file! I was looking for a closing parenthesis ')'.
  \n Please make sure you haven't missed one!"
  
  (* This is used *)
  let grab split_token char_list =
    (* if we find the token, split the list there *)
    let i_opt = List.findi ~f:(fun _ x -> x = split_token) char_list in
    match i_opt with
    | Some (i,_) -> let (x, xs) = List.split_n char_list i in
                    (x, (List.drop xs 1))
    | None       -> (char_list, [])

  let grab_assignment = grab Tokenizer.End

  (* This is the main parsing function *)
  let rec parse_linenum ~linenum x = 
    let open Tokenizer in
    match x with
    | QuoteString x :: rest ->
        Expression (StringEx x) :: (parse_linenum ~linenum:(linenum + 1) rest)

    | Float x :: rest ->
        Expression (Float x) :: (parse_linenum ~linenum:(linenum + 1) rest)

    | Newline :: rest ->
        (* increment line counts *)
        (parse_linenum ~linenum:(linenum + 1) rest)

    | Comment x :: rest ->
        (* ignore comments *)
        (parse_linenum ~linenum:linenum rest)

    | Box :: Symbol var_name :: Assignment :: rest  ->
        let (body, rest) =  grab_assignment rest in
        BoxAssign {var_name=var_name; expression = (parse_expr body) }
        ::
        (parse_linenum ~linenum:linenum rest)
    | End :: _ | [] -> []
    | x::rest -> 
        (BadToken (linenum, (to_string x))) :: (parse_linenum ~linenum:linenum rest)

  and parse_expr x = match x with
  | _ -> Float 2.0

  let parse = parse_linenum ~linenum:1


  let print_tree _ = ()
end
