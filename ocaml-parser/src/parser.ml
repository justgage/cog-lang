module Parser = struct
  open Core.Std
  open Tokenizer

  type symbol

  type boolean = bool

  type errors = 
    | WrongExpression

  type expression =
    | IfEx of if_ex
    | Operator of operator (* infix expressions *)
    | BoxDef of box_set
    | BoxAssign of box_set
    | Display of display
    | Expression of expression
    | FunctionExec of function_exec
    | FunctionDef of function_def
    | Repeat of repeat
    | Until of until
    | StringEx of string
    | Float of float
    | BadToken of bad_token 
    | ParenExp of expression
    | Boolean of bool
    | List of expression list 
    | VarGet of string
    | NoExpression
    | Error of errors
  and operator = { 
    symbol: string;
    lhand : expression;
    rhand : expression;
  }
  and box_set = {
    new_var_name : string;
    contents : expression;
    def_context : expression;
  }
  and display = {
    display_args : expression list;
    display_context : expression;
  }
  and if_ex = {
    condition : expression;
    true_body : expression;
    else_body : expression;
    if_context : expression;
  }
  and function_exec = {
    name : string;
    args : expression list;
  }
  and until = {
    done_condition : expression;
    body : expression;
    after : expression;
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
  and bad_token = {
    bad_token : Tokenizer.token;
    bad_context : expression;
  }
  and ast = expression (* abstract syntax tree *)

  (********** TYPES END **********)


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
  let rec split_on_commas args = 
    let module T = Tokenizer in
    match split ~on_token:T.Comma args with
    | (x,[]) -> [x]
    | (x,rest) -> x :: split_on_commas rest



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
          contents     = parse body;
          def_context  = parse rest;
        }
    | T.Symbol var_name :: T.Assignment :: rest  ->
        let (body, rest) = split ~on_token:T.Newline rest in
        BoxAssign {
          new_var_name = var_name;
          contents     = parse body;
          def_context  = parse rest;
        }

    | T.If :: rest -> 
        let (condition, rest) = split ~on_token:T.Then rest in
        let (body, rest)      = split ~on_token:T.Else    rest in
        let (else_body, rest) = split ~on_token:T.End     rest in
        let body_parsed       = parse body in
        let else_body_parsed  = parse else_body in
        IfEx {  
          condition = parse condition;
          true_body = body_parsed;
          else_body = else_body_parsed;
          if_context = parse rest;
        } 

    | T.Display :: T.OpenRound :: rest -> 
        let (args, rest) = split ~on_token:T.ClosingRound rest in
        Display {
          display_args = split_args_parse args;
          display_context = parse rest
        }

    (* Function Call *)
    | T.Symbol name :: T.OpenRound :: rest -> 
        let (args, rest) =  split ~on_token:T.ClosingRound rest in
        FunctionExec { 
          name = name;
          args = split_args_parse args;
        }

    (* List *)
    | T.OpenSquare :: rest -> 
        let (list_blob, _) =  split ~on_token:T.ClosingSquare rest in
        let list_items = list_blob |> split_on_commas in
        List (List.map ~f:parse list_items)

    | T.Symbol x :: rest -> VarGet x

    | T.Until::rest -> 
        let (condition, rest) = split ~on_token:T.Then rest in
        let (body, rest) = split ~on_token:T.End rest in
        Until {
          done_condition = parse condition;
          body = parse body;
          after = parse rest;
        }

    | x::rest -> 
        BadToken {
          bad_token = x; 
          bad_context = parse rest; 
        }

    | [] -> NoExpression (* best way to handle it? *)
  and split_args_parse args = 
    split_on_commas args |> List.map ~f:parse
    (* ---------------- end of main parser ---------------------*)

  let rec print_tree expression = 
    let open Printf in
    match expression with
    | Operator this -> sprintf "<operator>"
    | IfEx this -> 
        sprintf "if %s then \n %s \n else \n %s end %s" 
          (print_tree this.condition)
          (print_tree this.true_body)
          (print_tree this.else_body)
          (print_tree this.if_context)
    | BoxDef this -> sprintf "box %s = " "foobar"
    | BoxAssign this -> sprintf "%s = " "foobar"
    | Display this -> sprintf "<TO BE WRITTEN>" 
    | Expression x-> sprintf "<TO BE WRITTEN>" 
    | FunctionExec this -> sprintf "<TO BE WRITTEN>" 
    | FunctionDef this -> sprintf "<TO BE WRITTEN>" 
    | Repeat this -> sprintf "<TO BE WRITTEN>" 
    | Until this -> sprintf "<TO BE WRITTEN>" 
    | StringEx this -> sprintf "<TO BE WRITTEN>" 
    | Float this -> sprintf "<TO BE WRITTEN>" 
    | BadToken this -> sprintf "<TO BE WRITTEN>" 
    | ParenExp this -> sprintf "<TO BE WRITTEN>" 
    | Boolean this -> sprintf "<TO BE WRITTEN>" 
    | List this -> sprintf "<TO BE WRITTEN>" 
    | VarGet this -> sprintf "<TO BE WRITTEN>" 
    | NoExpression  -> sprintf "<TO BE WRITTEN>" 
    | Error this -> sprintf "<TO BE WRITTEN>" 
end
