module Parser : sig
  type symbol
  type user_func
  type keyword
  type ast (* abstract syntax tree *)

  open Tokenizer

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

  (* things that return a real value *)
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

  (* things that return io_side_effects *)
  type statement =
    | Display of string
    | BoxDef of (symbol * expression)
    | BoxAssign of box_assign
    | RepeatTill
    | Expression of expression
    | BadToken of int * string 


  type function_def = {
    name : symbol;
    args : expression list;
    body : expression list; (* or statement? *)
  }

  val parse : Tokenizer.token list -> ast
  val parse_linenum : linenum:int -> Tokenizer.token list ->  ast
  val print_tree : ast -> unit

end
