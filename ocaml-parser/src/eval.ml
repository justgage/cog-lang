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


(* Cog core lib 0 .0*)

(* this will display values of the system (PrattParser.term) *)
let rec display result =
  let module P = PrattParser in
  match result with
  | NoOp -> printf "warning: noop \n"
  | Values vs -> List.iter ~f:display vs
  | Value value -> begin
      match value with
      | P.Float f -> printf "%f" f
      | P.QuoteString s -> printf "\"%s\"" s
      | P.Boolean b -> printf "%s" (if b then "true" else "false")
      | _ -> printf  "Woops, looks like there's a problem with that code so I am unable to print this. Later I plan to add better error messaging but so far this is what I get. Sorry."
    end


let rec eval (tree : PrattParser.ast) : eval_result = begin
  let module P = PrattParser in
  match tree with
  | P.Blank ->
    NoOp
  | P.Statements statements ->
    Values (List.map ~f: eval statements)
  | P.Term x -> Value x
  | P.PrefixOperator x -> eval_prefix x
  | P.IfStatement ifs -> if_eval ifs
  | P.InfixOperator op -> NoOp
  | P.Assignment x -> NoOp
end

(* evaluates things that are prefix operators *)
and eval_prefix prefix =
  let module T = Tokenizer in
  let open PrattParser in
  match prefix.token_pre with
  | Tokenizer.Minus ->
    let result = eval prefix.right_pre in
    (match result with
    | x -> failwith "cog eval: this is not a prefix operator")
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


