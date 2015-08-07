module Parser : sig
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

  val parse : Tokenizer.token list -> ast
  val print_tree : ast -> unit
end
