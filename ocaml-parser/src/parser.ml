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


type expression =
  | FunctionExec of function_exec
  | Adition of (expression * expression)
  | Division of (expression * expression)
  | Boolean of boolean
  | String of string


type statement  =
  | Display of string
  | BoxDef of (symbol * expression)
  | BoxAssign
  | RepeatTill
  | Expression of expression

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


let rec parse x = 
  let open Tokenizer in
  match x with
  (* function call *)
  | Symbol a :: OpenRound :: s :: ClosingRound :: rest ->
      Display s
  | x::_ -> failwith (Printf.sprintf "There seems to be an error in parsing %s" (Tokenizer.to_string x))
  | [] -> []


let print_tree x = ()


