type l (* for lists *)

type symbol

type boolean =
  | True
  | False

type bolean_expression =
  | LessThan of (boolean * boolean)
  | GreaterThan of (boolean * boolean)
  | LessThanOrEqual of (boolean * boolean)
  | GreaterThanOrEqual of (boolean * boolean)

type user_func
type built_in


type function_exec = 
  | BuiltIn of built_in
  | UserFunc of user_func


type expression =
  | FunctionExec of function_exec
  | Adition of (expression * expression)
  | Division of (expression * expression)
  | Boolean of boolean


type statement =
  | Display
  | BoxDef of (symbol * expression)
  | BoxAssign
  | RepeatTill
  | Expression of expression


type function_def = {
  name : symbol;
  args : expression list;
  body : statement list;
}

type token =
  | String of string
  | Float of float
  | Expression of expression
  | Boolean of boolean
  | Whitespace of string
  | List of l
  | Statement of statement
  | End (* end tags *)

(* type ast = token : l *)

val from_str_list : bytes list -> token list
