module PrattParser = struct
  open Core.Std
  open Printf
  open Tokenizer
  open Result.Monad_infix

  let (|>=) ex f =  ex >>= fun x -> f x

  let print_end name x = (* printf "<- %s\n" name; *) x

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

  let expected_err exp got = Error (err @@ sprintf "Expected a `%s` but got %s" exp got)

  (**
   * This is what represents the parsed state
   *)
  type parse_state = 
    { 
      parsed : ast;
      rest : Tokenizer.token list;
     }
  type parse_monad = (parse_state, error) Core.Result.t

  (* END OF TYPES *)

  let rec ast_to_string op : string =
    begin match op with 
    | InfixOperator infix -> 
        sprintf "(%s  %s  %s)" 
          (ast_to_string infix.left)
          (Tokenizer.to_string infix.token)
          (ast_to_string infix.right)
    | Term x -> begin
      match x with 
      | Float f -> sprintf "%.f" f;
      end
      | Blank -> sprintf "<blank>"
    end


  let scope_rest =
    List.take_while

  let set_parsed new_val p : parse_monad =
    Result.return {
    parsed = new_val;
    rest = p.rest;
  }

  let set_rest new_val p  = Result.return {
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
  | x        -> failwith ("lbp called on something that isn't good" ^ (Tokenizer.to_string x))

let rbp token = 
  let module T = Tokenizer in
  match token with
  | T.Slash  -> 20
  | T.Star   -> 20
  | T.Plus   -> 10
  | T.Minus  -> 10
  | x        -> failwith ("lbp called on something that isn't good" ^ (Tokenizer.to_string x))

let advance p = 
    match List.hd p.rest with
      |  None -> expected_err (sprintf "something after %s" @@ ast_to_string p.parsed) "nothing"
      |  Some head -> Ok {
        parsed = p.parsed;
        rest   = List.drop p.rest 1;
      } 

let is_more p =  p.rest <> []
let next p = List.hd p.rest
let bigger p rbp = 
  match next p with
  | Some token -> rbp < (lbp token)
  | None -> false (* to end progression *)

let print_p name p rbp = 
  printf "%s (%d) : " name rbp ;
  printf "   tokens="; Tokenizer.print_tokens p.rest;
  printf "   parsed=%s\n" (ast_to_string p.parsed)

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
      left = prev_t.led(left) // old_left
      // these two are set to the new stuff

    return left

  *)
let rec power_loop (left_state : parse_state) ~rbp  = 
  print_p "-> power_loop!" left_state rbp;
  if is_more left_state && bigger left_state rbp then (
    (* printf "   There's more big tokens! \n"; *)
    led left_state left_state >>= fun left -> 
      (* rec -> *) power_loop left ~rbp 
  )
  else (
    printf "<- power: There's no more big tokens \n";
    Result.return left_state
  )

and expression ?(rbp = 0) state : parse_monad = 
  print_p "-> expression" state rbp;
  nud state >>= fun left -> 
  state |> advance  >>= fun infix_state -> (
    power_loop infix_state ~rbp
  )
  |> print_end "expression"

and nud p : parse_monad = 
  let module T = Tokenizer in
  (match p.rest with
  | T.Float f :: rest -> (
      p
      |> set_parsed (Term (Float f))  
      |>= advance
  )
  | x :: _ -> Error (err ("nud: Token was not a literal or a prefix operator! -->" ^ (T.to_string x)))
  | []     -> Error (err "nud: I was expecting a token! But you gave me none :,-(")
  )
  |> print_end "nud"


and led left_m next_state : parse_monad =
  printf "->  led  \n";
  let module T = Tokenizer in
  let infix_option = next(next_state) in
  match infix_option with
  | None -> Error (err "I was especting some sort of infix/postfix operator but got an end of file")
  | Some infix -> 
  match T.operator_type infix with
  | T.InfixOperator -> (
    (* get right hand *)
    next_state
    |> advance (* get rid of infix operator *)
    |>= expression ~rbp:(rbp infix) 
    >>= fun right ->
      print_p ("<- led of operator " ^ (Tokenizer.to_string infix)) right (rbp infix);
      Ok
      {
        rest = right.rest;
        parsed = InfixOperator {
          token = infix;
          left = left_m.parsed;
          right = right.parsed;
        };
      }
  )
  | _ -> expected_err "led: some sort if infix operator like a `+`" "nothing"


let blank_parse : parse_state = 
  {
    parsed = Blank;
    rest = [];
  }


let begin_parse tlist =  
  blank_parse
  |> set_rest tlist 
  |>= expression


let next_higher _pm = Error (err "This is undefined 0 .0")

end
