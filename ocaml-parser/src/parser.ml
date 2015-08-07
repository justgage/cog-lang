module Parser = struct
  open Core.Std
  open Tokenizer

  type symbol

  type boolean = bool

  type expression =
    | IfEx of if_ex
    | Operator of operator (* infix expressions *)
    | BoxDef of box_def
    | BoxAssign of box_assign
    | Display of expression
    | Expression of expression
    | FunctionExec of function_exec
    | FunctionDef of function_def
    | Repeat of repeat
    | RepeatTill of repeat_till
    | StringEx of string
    | Float of float
    | BadToken of string 
    | ParenExp of expression
    | Boolean of bool
    | MissingExpression
  and operator = { 
    symbol: string;
    lhand : expression;
    rhand : expression;
  }
  and box_def = {
    new_var_name : string;
    contents : expression;
    def_context : expression;
  }
  and box_assign = {
    var_name : string;
    new_contents : expression;
    context : expression;
  } 
  and if_ex = {
    condition : expression;
    true_body : expression;
    else_body : expression;
  }
  and function_exec = {
    name : string;
    args : expression list;
  }
  and repeat_till = {
    done_condition : expression;
    body : expression;
  }
  and repeat = {
    times : float;
    repeated_body : expression;
  }
  and function_def = {
    func_name : symbol;
    new_args : expression;
    new_body : expression; 
  }
  and ast = expression (* abstract syntax tree *)

  (********** TYPES END **********)

  let is_func x = match x with
  | x :: Tokenizer.OpenRound :: rest -> true
  | _ -> false
  
  (* 
   *
   * returns: touple of left of split_token and right of split_token*)
  let split ~on_token tokens =
    (* if we find the token, split the list there *)
    let (inside, rest) = 
      tokens |> List.split_while ~f:((<>) on_token) in
    (inside, (List.drop rest 1))

  (* Splits args appart at the comma
   * returns a list of args *)
  let rec split_args args = 
    let module T = Tokenizer in
    match split ~on_token:T.Comma args with
    | (x,[]) -> [x]
    | (x,rest) -> x :: split_args rest

  (* This is the main parsing function *)
  let rec parse tokens = 
    let module T = Tokenizer in

    match tokens with
    | T.QuoteString x :: rest ->
        StringEx x

    | T.Float x :: rest ->
        Expression (Float x)

    | T.Newline :: rest ->
        (* increment line counts? *)
        (* ignore *)
        (parse  rest)

    | T.Comment x :: rest ->
        (* ignore *)
        (parse  rest)

    | T.Box :: T.Symbol var_name :: T.Assignment :: rest  ->
        let (body, rest) = split ~on_token:T.Newline rest in
        BoxDef {
          new_var_name = var_name; 
          contents = (parse body);
          def_context = (parse rest);
        }

    | T.If :: rest -> 
        let (condition, rest) =  split ~on_token:T.Newline rest in
        let (body, rest) =  split ~on_token:T.Else rest in
        let (else_body, rest) =  split ~on_token:T.End rest in
        let body_parsed = (parse body) in
        let else_body_parsed = (parse else_body) in
        IfEx {  
          condition = ( parse condition );
          true_body = body_parsed;
          else_body = else_body_parsed;
        } 


    | T.Display :: T.OpenRound :: rest -> 
        let (args, rest) =  split ~on_token:T.ClosingRound rest in
        Display (parse args)

    (* Function *)
    | T.Symbol name :: T.OpenRound :: rest -> 
        let (args, rest) =  split ~on_token:T.ClosingRound rest in
        FunctionExec { 
          name = name;
          args = split_args args |> List.map ~f:parse
        }

    | x::rest -> (BadToken (Tokenizer.to_string x))

    | [] -> MissingExpression (* best way to handle it? *)
    (* ---------------- end of main parser ---------------------*)

  let print_tree _ = ()
end
