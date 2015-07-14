type symbol

type user_func

type keyword

type boolean =
  | True
  | False

type bolean_expression =
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
  | Adition of (expression * expression)
  | Division of (expression * expression)
  | Boolean of boolean


(* things that return io_side_effects *)
type statement =
  | Display
  | BoxDef of (symbol * expression)
  | BoxAssign
  | RepeatTill
  | Expression of expression


type function_def = {
  name : symbol;
  args : expression list;
  body : expression list; (* or statment? *)
}

type ast

type parse = Tokenizer.token -> ast

