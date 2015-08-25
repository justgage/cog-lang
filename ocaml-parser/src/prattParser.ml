module PrattParser = struct
  open Core.Std
  open Printf
  open Tokenizer
  open Result.Monad_infix

  let (|>=) ex f =  ex >>= fun x -> f x

  (* NOTE:
    * p and pm are used to mean "parser monad" which is
    * an extension of the Result monad that represents
    * the states of either 
    *
    * Error "error value" 
    * or 
    * Ok parse_state
    *
    * the definition of these are below
    *)

  type term = 
    | Float of float


  type ast = (* represents the tree *)
    | Blank
    | Term of term
    | InfixOperator of infix_operator
  and infix_operator =  
    {
      token : Tokenizer.token;
      right : ast;
       left : ast;
    }

  type error = string

  let err st : error = st

  let getErr x = x

  let expected_err exp got = Error (err @@ sprintf "COG: Expected a `%s` but got %s" exp got)

  (**
   * This is what represents the parsed state
   *)
  type parse_state = 
    { 
      current_rbp : int;
      parsed : ast;
      rest : Tokenizer.token list;
     }
  type parse_monad = (parse_state, error) Core.Result.t

  (* END OF TYPES *)

  let set_rbp new_val p = Result.return {
    current_rbp = new_val;
    parsed = p.parsed;
    rest = p.rest;
  }

  let set_parsed new_val p : parse_monad =
    Result.return {
    current_rbp = p.current_rbp;
    parsed = new_val;
    rest = p.rest;
  }

  let set_rest new_val p  = Result.return {
    current_rbp = p.current_rbp;
    parsed = p.parsed;
    rest = new_val;
  }



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

let advance p = 
    match List.hd p.rest with
      |  None -> expected_err "something after this operator" "nothing"
      |  Some head -> Ok {
        current_rbp = p.current_rbp;
        parsed = p.parsed;
        rest   = List.drop p.rest 1;
  } 

let is_more p =  p.rest <> []
let next p = List.hd p.rest

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
let rec expression ?(rbp = 0) start_state : parse_monad = 
  pre_or_term start_state           >>= fun left        -> 
  advance @@ start_state >>= fun next_state  -> (
    if is_more next_state then
      parse_infix left next_state
    else
      Result.return left
  )
and pre_or_term p : parse_monad = 
  let module T = Tokenizer in
  (match p.rest with
  | T.Float f :: rest -> (
      p
      |> set_parsed (Term (Float f))  
      |>= advance
  )
  | x :: _ -> Error (err ("Token was not a literal or a prefix operator! -->" ^ (T.to_string x)))
  | []     -> Error (err "I was expecting a token! But you gave me none :,-(")
  )
and parse_infix left_m next_state : parse_monad =
  let module T = Tokenizer in
  let infix_option = next(next_state) in
  match infix_option with
  | None -> Error (err "I was especting some sort of infix operator but got an end of file")
  | Some infix -> 
  match T.operator_type infix with
  | T.InfixOperator -> (
    next_state
    |>  advance 
    |>= expression ~rbp:(rbp infix) 
    >>= fun right ->
      Ok
      {
        current_rbp = (rbp infix);
        rest = right.rest;
        parsed = InfixOperator {
          token = infix;
          left = left_m.parsed;
          right = right.parsed;
        };
      }
  )
  | _ -> expected_err "some sort if infix operator like a `+`" "nothing"


let blank_parse : parse_state = 
  {
    current_rbp = 0;
    parsed = Blank;
    rest = [];
  }


let begin_parse tlist =  
  blank_parse
  |> set_rest tlist 
  |>= expression


let next_higher _pm = Error (err "This is undefined 0 .0")

end
