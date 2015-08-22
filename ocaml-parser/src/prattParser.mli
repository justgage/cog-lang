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

(* Note that the token classes have several attributes 
 * (not all classes have all kinds of attributes):

    nud - this is the prefix handler we talked about. In this simple parser it exists
    only for the literals (the numbers)

    ~ OR ~

    lbp - the left binding power of the operator. For an infix operator, it tells
    us how strongly the operator binds to the argument at its left.
    led - the infix handler.

    *)

module PrattParser : sig
  open Core.Std
  open Tokenizer
  
  (* terminating character *)
  type term = 
    | Float of float
  
  (* represents the abstract syntax tree *)
  type ast = 
    | Term of term (* terminating character, eg: a float, a string, etc... *)
    | EndToken (* End of expression token, eg: for files or when it just ends
      ^--- should this be an error?
    *)
    | InfixOperator of infix_operator (* an infix operator like `+` *)
  and infix_operator =  
     {
        token : Tokenizer.token;
        right : ast;
         left : ast;
     }
  
  (* left & right binding power *)
  val lbp : Tokenizer.token ->  int
  val rbp : Tokenizer.token ->  int
  
  (* This is prefix operators or terminating characters *)
  val nud : Tokenizer.token -> (ast, string) Result.t (* IDK. *) 
  val expression : ?rbp:int -> tokens:Tokenizer.token list -> (ast, string) Result.t
end
