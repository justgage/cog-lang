open Core.Std
open OUnit2
open PrattParser
open Tokenizer
open PrattParser
open Eval
open Cog

let (=?=) l r = assert_equal ~printer:(fun (str : string) -> str) l r

let test_expr code expected _test_ctxt =
  let got = Cog.run_str code in
  (expected =?= got)

let print_primitives ctx = todo "not yet implemented"

let basic_if = test_expr "if true then 1 else 2 end" "1.000000"
let basic_if2 = test_expr "if not true then 1 else 2 end" "2.000000"
let basic_if3 = test_expr "if not true then 1 else 2 end" "2.000000"

let boolean ctx = todo "not yet implemented"

let repeat ctx = todo "not yet implemented"

let boxes ctx = todo "not yet implemented"

let define_funcs ctx = todo "not yet implemented"

let run_funcs ctx = todo "not yet implemented"

let equality_for_lists ctx = todo "not yet implemented"

let equality_for_strings ctx = todo "not yet implemented"

(* Name the test cases and group them together *)
let suite =
"suite">:::
 [
   "if basic" >:: basic_if;
   "if basic 2" >:: basic_if2;
   "if basic 3" >:: basic_if3;
   "repeat" >:: repeat;
   "boxes" >:: boxes;
   "define_funcs" >:: define_funcs;
   "run_funcs" >:: run_funcs;
   "equality_for_lists" >:: equality_for_lists;
   "equality_for_strings" >:: equality_for_strings;
 ]

let () =
  run_test_tt_main suite
