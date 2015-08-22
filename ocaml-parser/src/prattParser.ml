module PrattParser = struct
  open Core.Std
  open Core.Result.Monad_infix
  open Printf
  open Tokenizer

type term = 
  | Float of float


type ast = (* represents the tree *)
  | Term of term
  | InfixOperator of infix_operator
and infix_operator =  
   {
      token : Tokenizer.token;
      right : ast;
       left : ast;
   }


let nud (a_token : Tokenizer.token ) : ast = 
  let module T = Tokenizer in
  match a_token with
  | T.Float x -> Term (Float x) 
  | x -> (failwith ("Expecting a ending pattern but got operator, `" 
                ^ Tokenizer.to_string x ^ "`"))


let lbp token = 
  let module T = Tokenizer in
  match token with
  | T.Slash  -> 20
  | T.Star   -> 20
  | T.Plus   -> 10
  | T.Minus  -> 10
  | _        -> 0 (* TODO:  probably don't want this in the future *)

let rbp token = 
  let module T = Tokenizer in
  match token with
  | T.Slash  -> 20
  | T.Star   -> 20
  | T.Plus   -> 10
  | T.Minus  -> 10
  | _        -> 0 (* TODO:  probably don't want this in the future *)


let next ls = match ls with
 | x :: xs -> Some (x, xs)
 | []  -> None 

(**  next_lower(rbp, tokens, left)
while rbp < token.lbp:
  prev_t = token
  token = next()          // keep track
  left = prev_t.led(left) // set above

   it collects all tokens that have higher binding power
   before returning to the operator that called it
   side effects the next to move things along
 **)
let rec grab_higher rbp tokens left = 
  match tokens with
  | infix::tokens -> 
      if rbp > lbp(infix) then
        parse_infix left infix tokens
      else
        grab_higher rbp tokens left
  | [] -> None

(* 
 *  WHERE THE PARSER STARTS
 * 
  def expression(rbp=0):
    global token
    t = token              // current one
    left = t.nud()         // assume it's a prefix / term char
    token = next()         // move token marker to next one

    while rbp < token.lbp:
      prev_t = token
      token = next()          // side effects to continue
      left = prev_t.led(left) // set above

    return left

  *)
and expression ?(rbp = 0) ~(tokens : Tokenizer.token list) = 
  let open Option.Monad_infix in
  next tokens  >>= fun (t, tokens2) ->    (* grab the first token *)
  let left = (nud t) in
  next tokens2 >>= fun (token, tokens) -> (* grab the next token *)
  let new_left = grab_higher rbp tokens left in (* implicit returnof left *)
  match new_left with
  | None -> Some left
  | Some left2 -> Some (led t left2 tokens)


and parse_infix left infix right =
  let open Option.Monad_infix in
  let module T = Tokenizer in
  match T.operator_type infix with
  | T.InfixOperator -> 
  (expression right) >>= fun right ->
  Ok {
    token = infix;
    left = left;
    right = right;
   }
  | __ -> Error ("Expected infix operator but got: a normal number or character" )
  
(* def led(self, left): *)
  (*   right = expression(10) *)
  (*     return left + right *)
and led (token : Tokenizer.token) left_ast rest =  (* CHANGE MEEEE *)
  let module T = Tokenizer in
  match token with
  | (T.Slash | T.Star | T.Plus | T.Minus ) as t -> 
      expression ~rbp:(rbp t) ~tokens:rest >>= fun right_ast -> 
    Ok {
      token = t;
       left = left_ast;
      right =  right_ast
  }
  | x -> Error (sprintf "Not an expression %s" (T.to_string x))

  (*

class literal_token(object): return self.value
  *)

  (*

class operator_add_token(object):
  lbp = 10
      def led(self, left):
        right = expression(10)
          return left + right
  *)

  (*

class operator_mul_token(object):
  lbp = 20
      def led(self, left):
        return left * expression(20)

  *)

  (*

class end_token(object):
  lbp = 0 
  *)

end
