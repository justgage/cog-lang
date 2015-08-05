module Parser : sig
  open Core.Std
  open Tokenizer

  type ast (* abstract syntax tree *)
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
    body : expression list; (* or statement? *)
  }

  val parse : Tokenizer.token list -> ast
  val print_tree : ast -> unit

end
