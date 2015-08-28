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

  (* terminating character *)
  type term = 
    | Float of float

  (* abstract syntax tree *)
  type ast = 
    | Blank (* null :( *)
    | Term of term (* terminating character, eg: a float, a string, etc... *)
    | InfixOperator of infix_operator (* an infix operator like `+` *)
  and infix_operator =  
     {
        token : Tokenizer.token;
        right : ast;
         left : ast;
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
end
