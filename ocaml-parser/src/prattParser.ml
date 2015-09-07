module PrattParser = struct
  open Core.Std
  open Printf
  open Tokenizer
  open Result.Monad_infix

  let debugging = false

  (* the monadic |> *)
  let (|>=) ex f = ex >>= fun x -> f x


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

  type ast = (* represents the tree *)
    | Blank
    | Statements of ast list
    | Term of term
    | InfixOperator of infix_operator
    | PrefixOperator of prefix_operator
    | IfStatement of if_statement
    | Assignment of assignment
    | Repeat of repeat
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
  and parse_state =
    {
      parsed : ast;
      rest : Tokenizer.token list;
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
   and repeat =
     {
       times : ast;
       rep_body : ast;
     }

  type error = string

  let err st : error = st

  let getErr x = x

  let expected_err exp got = Error (err @@ sprintf "Expected a `%s` but got `%s`" exp got)

  (**
   * This is what represents the parsed state
   *)
  type parse_monad = (parse_state, error) Core.Result.t

  let (<|>) (l : parse_monad) (r : parse_monad) : parse_monad =
    match l with
    | Ok _ -> l
    | Error _  -> r

  (* END OF TYPES *)

  let blank_parse : parse_state =
    {
      parsed = Blank;
      rest = [];
    }

  let rec ast_to_string op : string =
    begin match op with
    | Statements ls ->
        (
        List.fold
        ls
        ~init:""
        ~f: (fun left right ->  left ^ (ast_to_string right))
        )
    | InfixOperator infix -> (
        match infix.token with
        | Tokenizer.OpenRound ->
          sprintf "%s(%s)"
            (ast_to_string infix.left)
            (ast_to_string infix.right)
        | Tokenizer.Comma ->
          sprintf "%s, %s"
            (ast_to_string infix.left)
            (ast_to_string infix.right)
        | _ ->
          sprintf "(%s  %s  %s)"
            (ast_to_string infix.left)
            (Tokenizer.to_string infix.token)
            (ast_to_string infix.right)
      )
    | PrefixOperator prefix ->
        sprintf "(%s %s)"
          (Tokenizer.to_string prefix.token_pre)
          (ast_to_string prefix.right_pre)
    | Term x -> begin
      match x with
      | Float f -> sprintf "%.f" f;
      | QuoteString s -> sprintf "\"%s\"" s
      | Symbol s -> sprintf "%s" s
      | Boolean b -> sprintf "%b" b
      | List x -> sprintf "[%s]" (ast_to_string x)
      end
    | Blank -> sprintf "<blank>"
    | IfStatement ifs ->
        sprintf
        "if %s \n  then %s \n  else %s end"
          (ast_to_string ifs.condition)
          (ast_to_string ifs.true_branch)
          (ast_to_string ifs.false_branch)
    | Assignment {var_name; set_to ; context} ->
      sprintf "box %s = %s;\n%s"
        (var_name) (ast_to_string set_to) (ast_to_string context)
    | Repeat rep ->
      sprintf "repeat %s ->\n\t%s" (ast_to_string rep.times) (ast_to_string rep.rep_body)
    end

  let to_string pm =
    match pm with
    | Ok {parsed; _} -> sprintf "printing result:\n %s\n" (ast_to_string parsed)
    | Error x -> sprintf "Sorry that AST is broken\n\terror: %s" x

  let print pm = printf "%s" (to_string pm)

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
    | T.OpenRound -> 300
    | (T.Slash | T.Star) -> 200
    | (T.Plus | T.Minus) -> 100
    | T.LogicAnd -> 60
    | T.LogicOr  -> 50
    | T.OpenSquare -> 1
    | ( T.LessThan
      | T.LessThanOrEqual
      | T.Equal
      | T.GreaterThan
      | T.GreaterThanOrEqual) -> 40
    | ( T.ClosingRound
      | T.ClosingSquare
      | T.Then
      | T.Else
      | T.End
      | T.Newline
      | T.EndOfStatement
      | T.Arrow
      ) -> 0 (* always stop parsing *)
    | T.Comma -> 2 (*is this right?*)
    | (T.Assignment
      (* prefix operator *)
      | T.LogicNot
      (* nuds *)
      | T.QuoteString _
      | T.Boolean _
      | T.Box
      | T.CommentBegin
      | T.Comment _
      | T.DoubleQuote
      | T.Float _
      | T.FuncDef
      | T.If
      | T.Repeat
      | T.Display
      | T.Until
      | T.Symbol _) as x -> failwith ("lbp called on something that isn't good ->  " ^ (Tokenizer.to_string x))

  let rbp token =
    let module T = Tokenizer in
    match token with
    | (T.Slash | T.Star)  -> 200
    | (T.Plus | T.Minus)  -> 100
    | T.LogicNot -> 65
    | T.LogicAnd -> 60
    | T.LogicOr  -> 50
    | ( T.GreaterThan
      | T.GreaterThanOrEqual
      | T.LessThan
      | T.LessThanOrEqual
      | T.Equal) -> 40
    | T.Comma -> 1 (*is this right?*)
    | T.OpenRound -> 0
    | (T.Assignment
      |T.Boolean _
      |T.Box
      |T.ClosingRound
      |T.ClosingSquare
      |T.CommentBegin
      |T.Comment _
      |T.DoubleQuote
      |T.QuoteString _
      |T.Else
      |T.End
      |T.Float _
      |T.FuncDef
      |T.If
      |T.Newline
      |T.OpenSquare
      |T.Repeat
      |T.Display
      |T.Until
      |T.Symbol _
      |T.Then
      |T.EndOfStatement
      |T.Arrow
      )
    as x -> failwith ("rbp called on something that isn't good" ^ (Tokenizer.to_string_debug x))

  let advance p =
      match List.hd p.rest with
        |  None -> expected_err (sprintf "something after %s" @@ ast_to_string p.parsed) "nothing"
        |  Some _ -> Ok {
          parsed = p.parsed;
          rest   = List.drop p.rest 1;
        }

  let is_more_tokens p =  p.rest <> []

  (* gets the next token and returns an option *)
  let next p = List.hd p.rest

  (* checks to see if the curent token is the right one.
   * If so it advances. Else it errors. *)
  let match_next expected_token p =
    let exp_err = expected_err (Tokenizer.to_string_debug expected_token) in
    (* note the currying --^ *)
    match next p with
    | Some found_token ->
        if expected_token = found_token
        then p |> advance
        else exp_err (Tokenizer.to_string_debug found_token)
    | None ->
        exp_err "end of expression or file"

  let bigger p rbp =
    match next p with
    | Some token -> rbp < (lbp token)
    | None -> false (* to end progression *)

  let print_p name p rbp =
    if debugging then (
      printf "%s (%d) : " name rbp ;
      printf "   tokens="; Tokenizer.print_tokens p.rest;
      printf "   parsed=%s\n" (ast_to_string p.parsed)
    ) else ()

  (* The python code I was working off of
      while rbp < token.lbp:
        prev_t = token
        token = next()          // side effects to continue
        left = prev_t.led(left) // old_left
        // these two are set to the new stuff
    *)
  let rec power_loop (left_state : parse_state) ~rbp  =
    print_p "-> power_loop!" left_state rbp;
    if is_more_tokens left_state
       && bigger left_state rbp
        then (left_state
              |> led
              |>= power_loop ~rbp)
        else Result.return left_state

  (* The python code I was working off of
    def expression(rbp=0):
      global token
      t = token              // current one
      left = t.nud()         // assume it's a prefix / term char
      token = next()         // move token marker to next one

      // insert "power_loop" here

      return left

    *)
  and expression ?(rbp = 0) state : parse_monad =
    print_p "-> expression" state rbp;
    state
    |> nud
    |>= power_loop ~rbp

  and nud p : parse_monad =
    let module T = Tokenizer in
    (match p.rest with
     | T.Float f :: _ ->
        p
        |> set_parsed (Term (Float f))
        |>= advance

    | T.QuoteString s :: _ ->
        p
        |> set_parsed (Term (QuoteString s))
        |>= advance

    | T.Symbol s :: _ ->
        p
        |> set_parsed (Term (Symbol s))
        |>= advance

    | T.Boolean b :: _ ->
      let b = match b with
        | T.True -> true
        | T.False -> false in
        p
        |> set_parsed (Term (Boolean b))
        |>= advance

    | (T.LogicNot | T.Minus) as token :: _ ->
      add_prefix_operator p token

    | T.OpenRound :: _ ->
        p
        |>  advance
        |>= expression
        |>= match_next T.ClosingRound
    | T.OpenSquare :: _ ->
      p
      |> advance >>= fun exp ->
      ((exp |> match_next T.ClosingSquare)
       <|>
       (exp
        |> expression
        |>= match_next T.ClosingSquare))
      >>= fun inside ->
      inside |>
      set_parsed
        (PrefixOperator {
          token_pre = Tokenizer.OpenSquare;
          right_pre = inside.parsed;
        })

    | T.Repeat :: _ -> begin
        p
        |>  advance
        |>= expression
        >>= fun times_ex ->
        times_ex
        |> match_next T.Arrow
        |>= expression
        >>= fun body ->
        body
        |> set_parsed
        (Repeat {
          times = times_ex.parsed;
          rep_body = body.parsed;
        })


        end

    | T.If :: _ -> begin
        p
        |>  advance
        |>= expression
        |>= match_next T.Then
        >>= fun condition_p ->
          condition_p
        |>  expression ~rbp:0
        |>= match_next T.Else
        >>= fun true_branch ->
          true_branch
        |>  expression ~rbp:0
        |>= match_next T.End
        >>= fun false_branch ->
          false_branch
          |> set_parsed (IfStatement {
            condition = condition_p.parsed;
            true_branch = true_branch.parsed;
            false_branch = false_branch.parsed;
        })
      end

    | T.Box :: _ -> (
      p
      |> advance >>= fun p_next ->
        match p_next.rest with
      | [] -> expected_err
                "A variable name (Symbol)"
                "end of file"
      | T.Symbol var_name :: _ ->
        p_next
        |> advance
        |>= match_next T.Assignment
        |>= expression >>= fun right ->
        right
        |> match_next T.EndOfStatement >>= fun p ->
        if p.rest = []
        then expected_err "An expression after the Box to use it in" "Nothing! :'("
        else
          expression p >>= fun ctx ->
          ctx |> set_parsed (Assignment {
              var_name = var_name;
              set_to = right.parsed;
              context = ctx.parsed})
      | got :: _ ->
        expected_err
          "A variable name (Symbol)"
          (T.to_string (got))
      )
    | (
      T.Assignment
      |T.ClosingRound
      |T.ClosingSquare
      |T.CommentBegin
      |T.Comment _
      |T.DoubleQuote
      |T.Else
      |T.End
      |T.GreaterThan
      |T.GreaterThanOrEqual
      |T.Equal
      |T.LessThan
      |T.LessThanOrEqual
      |T.LogicAnd
      |T.LogicOr
      |T.Newline
      |T.Plus
      |T.Slash
      |T.Star
      |T.Comma
      |T.Then
      |T.Display
      |T.Arrow
      |T.EndOfStatement
      (* Shouldn't be here *)
      |T.FuncDef
      |T.Until
      ) as x :: _ -> Error (err ("nud: Token was not a literal or a prefix operator! It was: " ^ (T.to_string x)))
    | []     -> Error (err "nud: I was expecting a literal or prefix operator! But there none :,-(")
    )
    and add_prefix_operator p token =
        p
        |> advance (* past LogicNot *)
        |>= expression ~rbp:(rbp token) >>= fun right_exp ->
        right_exp |> set_parsed (PrefixOperator {
            token_pre = token;
            right_pre = right_exp.parsed;
          })

    and led left_s : parse_monad =
      let module T = Tokenizer in
      let infix_option = next(left_s) in
      match infix_option with
      | None -> Error (err "I was especting some sort of infix/postfix operator but got an end of file")
      | Some infix ->
        match T.operator_type infix with
        | T.InfixOperator -> (
            (* get right hand *)
            left_s
            |> advance (* get rid of infix operator *)
            |>= expression ~rbp:(rbp infix)
            >>= fun right ->
            print_p ("<- led of operator " ^ (Tokenizer.to_string_debug infix)) right (rbp infix);
            Ok
              {
                rest = right.rest;
                parsed = InfixOperator {
                    token = infix;
                    left = left_s.parsed;
                    right = right.parsed;
                  };
              }
          )
        | T.PrefixOperator -> expected_err "led: some sort if infix operator like a `+`" "a Prefix operator"
        | T.Value -> expected_err "led: some sort if infix operator like a `+`" "a normal value"
        | T.OtherSyntax -> Error (err "The creator of this language needs to define this")

  let begin_parse tlist =
    blank_parse
    |> set_rest tlist
    |>= expression

end
