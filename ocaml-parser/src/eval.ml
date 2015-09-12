module Eval = struct

open Core.Std
open Tokenizer
open PrattParser


type eval_result =
  | Value of PrattParser.term
  | Values of eval_result list
  | List of eval_result list
  | FuncDef of func_def
  | NoOp
and func_def = {
  fd_name : string;
  fd_args : string list;
  fd_ast : PrattParser.ast;
}

type var_map = eval_result String.Map.t

(* BEGINING OF SCOPE STUFF *)
let scope : var_map Stack.t = Stack.create ()

let () = Stack.push scope (String.Map.empty)

let scope_begin () =
  match Stack.top scope with
  | Some parent_scope -> Stack.push scope parent_scope
  | None              -> failwith "Scope stack was empty, this should never happen and is almost surely a bug"

let scope_add key data =
  match (Stack.pop scope) with
  | None -> failwith "scope_add: no scope to add to! Probably a bug with popping too many times"
  | Some sc -> Stack.push scope (String.Map.add sc ~key ~data)

let scope_lookup key =
  match (Stack.top scope) with
  | None -> failwith "scope_lookup: no scope to look in! Probably a bug with popping too many times"
  | Some sc -> String.Map.find sc key

let scope_end () = let _ =  Stack.pop scope in ()

(* try stuff *)

let try_lookup func_name name  ~f:callback =
  match scope_lookup name with
  | None -> failwith (func_name ^ " > try_lookup: no varaible with name: " ^ name)
  | Some value -> value |> callback func_name


let rec try_float func_name eval_res =
  match eval_res with
  | Value v ->
    (
    match v with
    | PrattParser.Float f -> f
    | PrattParser.Symbol name ->
      try_lookup (func_name ^ "try_float") name ~f:try_float
    | _  ->
      failwith
        (func_name ^ " > try_float: Expected this to be a float but got  something else")
    )
  | Values _ ->
    failwith (func_name ^ "> try_float: Expected a float but got multiple")
  | NoOp ->
    failwith (func_name ^ "> try_float: Expected a float but got NoOp")

  | FuncDef _ ->
    failwith (func_name ^ "> try_float: Expected a float but got a Function")

  | List _ ->
    failwith (func_name ^ "> try_float: Expected a float but got a list")


let rec try_bool func_name eval_res =
  match eval_res with
  | Value v ->
    (
    match v with
    | PrattParser.Boolean b -> b
    | _  ->
      failwith
        (func_name ^ " > try_bool: Expected this to be a boolean (true or false) but got something else")
    )
  | Values _ -> failwith (func_name ^ "> try_bool: Expected a boolean but got multiple")
  | NoOp -> failwith (func_name ^ "> try_bool: Expected a boolean but got NoOp")
  | FuncDef _ -> failwith (func_name ^ "> try_bool: Expected a float but got a Function")
  | List _ -> failwith (func_name ^ "> try_bool: Expected a float but got a list")

(* makes values *)
let rec compare_pratt func_name comp_touple =
  let module P = PrattParser in
  match comp_touple with
  | P.Float l, P.Float r ->
    l = r
  | P.QuoteString l, P.QuoteString r ->
    l = r
  | P.List l, P.List r ->
    l = r
  | P.Boolean l, P.Boolean r ->
    l = r
  | l, r ->
    failwith (func_name ^ "These two types aren't compareable: "
              ^ (P.ast_to_string (P.Term l)) ^ "=/=" ^ (P.ast_to_string (P.Term r)))


(* this will display values of the system (PrattParser.term) *)
let rec to_string result =
  let module P = PrattParser in
  match result with
  | NoOp ->
    sprintf "\n"
    (* sprintf "warning: noop \n" *)
  | Values vs -> List.fold ~init:"" ~f:(fun acc next -> acc ^ "\n" ^ (to_string next)) vs
  | Value value -> begin
      match value with
      | P.Float f       -> sprintf "%.2f" f
      | P.QuoteString s -> sprintf "%s" s
      | P.Boolean b     -> sprintf "%s" (if b then "true" else "false")
      | P.Symbol var_name -> (
        match (scope_lookup var_name ) with
        | Some value -> value |> to_string
        | None -> failwith "Sorry that variable doesn't exist in this scope" ^ var_name
        )
      | _ as bad_p -> sprintf  "Welp, Trying to turn a pratt parser thing into a string that shouldn't have happened, it was: %s" (PrattParser.ast_to_string (P.Term bad_p))
    end
  | FuncDef fn -> sprintf "%s <func-def-code>" fn.fd_name
  | List lst ->
    sprintf "[%s]"
      (lst
       |> List.map ~f:to_string
       |> List.intersperse ~sep: ", "
       |> List.fold ~init:"" ~f:(^))

let print_op result =
  (printf "%s" (to_string result))

let display args =
  let str_list = List.map ~f:to_string args in
  printf "%s" (List.fold ~init:"" ~f:(^) str_list);
  NoOp

(* Cog core lib 0 .0*)

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
    | P.Term t -> (
      match t with
      | P.List lst -> List (List.map ~f:eval lst)
      | P.Symbol smb ->
        (match scope_lookup smb with
         | Some value -> value
         | None -> failwith "Sorry that variable isn't in the scope")
      | _ -> Value t
      )
    | P.PrefixOperator x -> eval_prefix x
    | P.IfStatement ifs -> if_eval ifs
    | P.InfixOperator op -> infix_eval op
    | P.Assignment box ->
        scope_add box.P.var_name
          (eval box.P.set_to); (* box assigned to *)
        eval box.P.context (* box's context *)
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
    let l = eval infix.left   |> try_float "infix_eval" in
    let r = eval infix.right  |> try_float "infix_eval" in
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
  | T.Equal -> (
    let {left; right} = infix in
    let l = eval left in
    let r = eval right in
    match (l, r) with
    | Value l, Value r -> (
      Value (PrattParser.Boolean (compare_pratt "infix_eval" (l, r)))
      )
    | List l, List r -> (
      Value (PrattParser.Boolean (l = r))
      )
    | _ -> failwith "Woah! those are not compareable"
    )

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
        | PrattParser.Boolean b -> (
            if b then (
              scope_begin ();
              let tb_result = eval tb in
              scope_end ();
              tb_result
            )
            else (
            scope_begin ();
            let rb_result = eval fb in
            scope_end ();
            rb_result
            )
          )
        | _ -> failwith "seems that the condition isn't a boolean! (evaled to somthing other than true or false)"
        end
    | _ -> failwith "This isn't a value! you can't put things like display inside the condiiton of an if statement"
end;;


