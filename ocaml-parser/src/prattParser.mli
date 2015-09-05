(**
 * Pratt termonology:
 *
 *     Name:
   *     The symbol name matches either a token’s
   *     name or value
 *     Lbp:
   *     The left binding power of this symbol.
   *     Determines if it will grab the previous
   *     expression.
 *     Nud:
   *     The function called
   *     when this symbol is executed with no
   *     arguments
 *     Led:
   *     The function called when this symbol
   *     is executed with the previous expression’s
   *     result.
 * Questions------------
   * How do we bind operators that are afterwards?
 *)

module PrattParser : sig
  open Core.Std
  open Tokenizer
  open Core.Result


  (**** TYPES ****)


  (* abstract syntax tree *)
  type ast =
    | Blank (* null :( *)
    | Statements of ast list
    | Term of term (* terminating character, eg: a float, a string, etc... *)
    | InfixOperator of infix_operator (* an infix operator like `+` *)
    | PrefixOperator of prefix_operator
    | IfStatement of if_statement
    | Assignment of assignment
  (* terminating character *)
  and term =
    | Float of float
    | QuoteString of string
    | Symbol of string
    | Boolean of bool
    | List of ast  (* <-- Should this be a list? *)
  and infix_operator =
     {
        token : Tokenizer.token;
        right : ast;
         left : ast;
     }
  and prefix_operator =
    {
      token_pre : Tokenizer.token;
      right_pre : ast;
    }
   and if_statement =
     {
       condition: ast;
       true_branch : ast;
       false_branch : ast;
     }
   and assignment =
     {
       var_name : string;
       set_to : ast;
       context : ast;
     }

  type error
  val getErr : error -> string

  (**
   * This is what represents the parsed state
   *)
  type parse_state =
    {
      parsed : ast;
      rest : Tokenizer.token list;
     }

  type parse_monad = (parse_state, error) Result.t

  (* the monadic |> *)
  val (|>=) : parse_monad -> (parse_state -> parse_monad) -> parse_monad

  (**** FUNCTIONS ****)

  (* left binding power *)
  val lbp : Tokenizer.token ->  int

  (* right binding power *)
  val rbp : Tokenizer.token ->  int


  (* This is prefix operators
   * or terminating characters *)
  val nud : parse_state -> parse_monad

  (*
   * This will be the main parsing function
   * *)
  val expression : ?rbp:int -> parse_state -> parse_monad

  val begin_parse : Tokenizer.token list -> parse_monad

  (* get the next expression that's lower than *)
  val next_higher : parse_state -> parse_monad

  val print : parse_monad -> unit
  val to_string : parse_monad -> string
end
