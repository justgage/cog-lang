module Parser : sig
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
    if_contex : expression;
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

  val parse : Tokenizer.token list -> ast
  val print_tree : ast -> unit
end
