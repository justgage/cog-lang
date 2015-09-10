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
    | _  ->
      failwith
        (func_name ^ " > try_float: Expected this to be a float but got  something else")
    )
  | Values _ -> failwith (func_name ^ "> try_float: Expected a value but got multiple")
  | NoOp -> failwith (func_name ^ "> try_float: Expected a value but got NoOp")


let try_bool func_name eval_res =
  match eval_res with
  | Value v ->
    (
    match v with
    | PrattParser.Boolean b -> b
    | _  ->
      failwith
        (func_name ^ " > try_bool: Expected this to be a boolean (true or false) but got something else")
    )
  | Values _ -> failwith (func_name ^ "> try_float: Expected a value but got multiple")
  | NoOp -> failwith (func_name ^ "> try_float: Expected a value but got NoOp")

(* Cog core lib 0 .0*)

(* this will display values of the system (PrattParser.term) *)
let rec to_string result =
  let module P = PrattParser in
  match result with
  | NoOp -> sprintf "warning: noop \n"
  | Values vs -> List.fold ~init:"" ~f:(fun acc next -> acc ^ "\n" ^ (to_string next)) vs
  | Value value -> begin
      match value with
      | P.Float f       -> sprintf "%.2f" f
      | P.QuoteString s -> sprintf "%s" s
      | P.Boolean b     -> sprintf "%s" (if b then "true" else "false")
      | _               -> sprintf  "Woops, looks like there's a problem with that code so I am unable to print this. Later I plan to add better error messaging but so far this is what you get. Sorry.\n"
    end

let print_op result =
  (printf "%s" (to_string result))

let display args =
  let str_list = List.map ~f:to_string args in
  printf "%s" (List.fold ~init:"" ~f:(^) str_list);
  NoOp

let display_newline args =
  display (List.append args [Value (PrattParser.QuoteString "\n")])

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
    | P.FuncCall fn -> func_eval fn
  end

and infix_eval infix =
  let module T = Tokenizer in
  let open PrattParser in

  let apply_bool_op infix op =
    let l = eval infix.left |> try_bool "infix_eval" in
    let r = eval infix.right |> try_bool "infix_eval" in
    Value (PrattParser.Boolean (op l r))

  and apply_compare infix op =
    let l = eval infix.left |> try_float "infix_eval" in
    let r = eval infix.right |> try_float "infix_eval" in
    Value (PrattParser.Boolean (op l r))

  and apply_float_op infix op =
    let l = eval infix.left |> try_float "infix_eval" in
    let r = eval infix.right |> try_float "infix_eval" in
    Value (PrattParser.Float (op l r))
  in

  let module T = Tokenizer in
  match infix.token with
  (* float operations *)
  | T.Plus ->
    apply_float_op infix ( +. )

  | T.Minus ->
    apply_float_op infix ( -. )

  | T.Slash ->
    apply_float_op infix ( /. )

  | T.Star ->
    apply_float_op infix ( *. )

  (* float operations *)
  | T.Equal ->
    apply_compare infix ( = )

  | T.LessThan ->
    apply_compare infix ( <. )

  | T.LessThanOrEqual ->
    apply_compare infix ( <=. )

  | T.GreaterThan ->
    apply_compare infix ( >. )

  | T.GreaterThanOrEqual ->
    apply_compare infix ( >=. )

  (* logic operations *)
  | T.LogicAnd ->
    apply_bool_op infix ( && )

  | T.LogicOr ->
    apply_bool_op infix ( && )

  | _ as t -> failwith ((T.to_string t) ^ "isn't an implemented operator yet!")

and func_eval func =
  let module P = PrattParser in
  let args = List.map ~f:eval (func.P.func_args) in
  match func.P.func_name with
  | "display" -> display args
  | "display_newline" -> display_newline args
  | x         -> failwith ("function not defined: " ^ x)



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

  let apply_bool_op prefix op =
    let l = eval prefix.right_pre |> try_bool "prefix_eval" in
    Value (PrattParser.Boolean (op l))

  and apply_float_op prefix op =
    let l = eval prefix.right_pre |> try_float "prefix_eval" in
    Value (PrattParser.Float (op l))
  in

  match prefix.token_pre with
  | Tokenizer.Minus ->
    let negate x = x *. -1. in
    apply_float_op prefix negate
  | Tokenizer.LogicNot -> apply_bool_op prefix (not)
  | _ -> failwith "cog eval: this is not a prefix operator"

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


