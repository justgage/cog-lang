module Eval = struct

open Core.Std
open Tokenizer
open PrattParser

type error =
  | ParserError of string
  | EvalError of string

type eval_result =
  | Value of PrattParser.term
  | Values of eval_result list
  | NoOp

let try_float func_name eval_res =
  match eval_res with
  | Value v ->
    (
    match v with
    | PrattParser.Float f -> f
    | _  as item ->
      failwith
        (func_name ^ " > try_float: Expected this to be a float but got  something else")
    )
  | Values _ -> failwith (func_name ^ "> try_float: Expected a value but got multiple")
  | NoOp -> failwith (func_name ^ "> try_float: Expected a value but got NoOp")


(* Cog core lib 0 .0*)

(* this will display values of the system (PrattParser.term) *)
let rec display result =
  let module P = PrattParser in
  match result with
  | NoOp -> printf "warning: noop \n"
  | Values vs -> List.iter ~f:display vs
  | Value value -> begin
      match value with
      | P.Float f       -> printf "%f" f
      | P.QuoteString s -> printf "%s" s
      | P.Boolean b     -> printf "%s" (if b then "true" else "false")
      | _               -> printf  "Woops, looks like there's a problem with that code so I am unable to print this. Later I plan to add better error messaging but so far this is what you get. Sorry.\n"
    end


(* evaluates the Cog code *)
let rec eval (tree : PrattParser.ast) : eval_result =
  begin
    let module P = PrattParser in
    match tree with
    | P.Blank -> NoOp
    | P.Statements statements ->
      Values (List.map ~f: eval statements)
    | P.Term x -> Value x
    | P.PrefixOperator x -> eval_prefix x
    | P.IfStatement ifs -> if_eval ifs
    | P.InfixOperator op -> infix_eval op
    | P.Assignment x -> NoOp
    | P.Repeat r -> repeat_eval r
  end

and infix_eval infix =
  let module T = Tokenizer in
  let open PrattParser in
  match infix.token with
  | T.OpenRound -> func_eval infix

  | Tokenizer.Plus ->
    let l = eval infix.left |> try_float "infix_eval" in
    let r = eval infix.right |> try_float "infix_eval" in
    Value (PrattParser.Float (l +. r))

  | Tokenizer.Minus ->
    let l = eval infix.left |> try_float "infix_eval" in
    let r = eval infix.right |> try_float "infix_eval" in
    Value (PrattParser.Float (l -. r))

  | Tokenizer.Slash ->
    let l = eval infix.left |> try_float "infix_eval" in
    let r = eval infix.right |> try_float "infix_eval" in
    Value (PrattParser.Float (l /. r))

  | Tokenizer.Star ->
    let l = eval infix.left |> try_float "infix_eval" in
    let r = eval infix.right |> try_float "infix_eval" in
    Value (PrattParser.Float (l *. r))


  | _ as t -> failwith ((Tokenizer.to_string t) ^ "isn't an implemented operator yet!")

and func_eval func = NoOp


and repeat_eval r =
    begin
      let times = eval PrattParser.(r.times) in
      let rec repeat times exp : unit =
        if times >. 0. then
          let _ = eval exp in
          repeat (times -. 1.) exp
      in
      (match times with
       | Value v -> (
           match v with
           | PrattParser.Float times ->
             repeat times PrattParser.(r.rep_body);
             NoOp
           | _ -> failwith "repeat expects it's condition to evaluate to a float"
         )
       | _ -> failwith "repeat expects a value as it's condition"
      )
    end

(* evaluates prefix operators *)
and eval_prefix prefix =
  let module T = Tokenizer in
  let open PrattParser in
  match prefix.token_pre with
  | Tokenizer.Minus ->
    (
      let result = eval prefix.right_pre in
      let negate v = (
          match v with
          | Float v -> Value (Float (-.v))
          | x -> failwith "tried to negate something that wasn't a number")
      in (
        match result with
        | Value v -> negate v
        | NoOp -> failwith "trying to negate somthign that isn't a value like 'display'"
        | Values _ -> failwith "Trying to eval more than just a number"
      )
    )
    | x -> failwith "cog eval: this is not a prefix operator"




and if_eval ifs =
  let open PrattParser in
    let cond = eval ifs.condition in
    let tb   = ifs.true_branch    in
    let fb   = ifs.false_branch   in
    match cond with
    | Value b -> begin
        match b with
        | PrattParser.Boolean b ->
          if b then
            eval tb
          else
            eval fb
        | _ -> failwith "seems that the condition isn't a boolean! (evaled to somthing other than true or false)"
        end
    | _ -> failwith "This isn't a value! you can't put things like display inside the condiiton of an if statement"
end;;


