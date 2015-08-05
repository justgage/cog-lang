module Parser = struct
  open Core.Std
  open Tokenizer


  type symbol

  type boolean =
    | True
    | False

  type boolean_expression =
    | LessThan of           ( boolean_expression * boolean_expression)
    | GreaterThan of        ( boolean_expression * boolean_expression)
    | LessThanOrEqual of    ( boolean_expression * boolean_expression)
    | GreaterThanOrEqual of ( boolean_expression * boolean_expression)
    | Boolean of boolean


  type expression =
    | Addition of (expression * expression)
    | BadToken of string 
    | BooleanEx of  boolean_expression
    | BoxAssign of box_assign
    | BoxDef of (symbol * expression)
    | Display of expression
    | Division of (expression * expression)
    | Expression of expression
    | Float of float
    | FunctionExec of function_exec
    | IfEx of if_ex
    | RepeatTill
    | StringEx of string
  and box_assign = {
    var_name : string;
    expression : expression;
  } 
  and if_ex = {
    condition : boolean_expression Option.t;
    body : expression list;
    else_body : expression list;
  }
  (* a function call *)
 and function_exec = {
     name : string;
    args : expression list;
  }


  type function_def = {
    name : symbol;
    args : expression list;
    body : expression list;
  }

  type ast = expression list


  let is_func x = match x with
  | x :: Tokenizer.OpenRound :: rest -> true
  | _ -> false

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

  (* This is the main parsing function *)
  let rec parse x = 
    let module T = Tokenizer in
    
    match x with
    | T.QuoteString x :: rest ->
        Expression (StringEx x) :: (parse rest)

    | T.Float x :: rest ->
        Expression (Float x) :: (parse rest)

    | T.Newline :: rest ->
        (* increment line counts *)
        (parse  rest)

    | T.Comment x :: rest ->
        (* ignore comments *)
        (parse  rest)

    | T.Box :: T.Symbol var_name :: T.Assignment :: rest  ->
        let (body, rest) =  grab T.Newline rest in
        BoxAssign {var_name=var_name; expression = (parseexpr body) }
        ::
        (parse rest)


    | T.If :: rest -> 
        let (condition, rest) =  grab T.Newline rest in
        let (body, rest) =  grab T.Else rest in
        let (else_body, rest) =  grab T.End rest in
        let body_parsed = (parse body) in
        let else_body_parsed = (parse else_body) in
        IfEx {  
          condition = parseboolean condition ;
          body = body_parsed;
          else_body = else_body_parsed;
        } :: parse rest

    | T.End :: _ | [] -> []

    | T.Display :: T.OpenRound :: rest -> 
        let (args, rest) =  grab T.ClosingRound rest in
        Display (parseexpr args) :: (parse rest)

    (* Function *)
    | T.Symbol name :: T.OpenRound :: rest -> 
        let (args, rest) =  grab T.ClosingRound rest in
        FunctionExec { 
          name = name;
          args = (parse args)
        } :: (parse rest)

    | x::rest -> (BadToken (Tokenizer.to_string x)) :: parse rest
    (* ---------------- end of main parser ---------------------*)

  and parseboolean x = match x with
  | _ -> Some (Boolean False)

  and parseexpr x = match x with
  | _ -> Float 2.0



  let print_tree _ = ()
end
