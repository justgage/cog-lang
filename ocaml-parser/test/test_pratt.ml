open OUnit2;;
open PrattParser;;
open Tokenizer;;

let trial_tokens = 
  let module T =  Tokenizer in
  [T.Float 12.; T.Plus; T.Float 2.]
let basic_parser test_ctxt = 
  assert_equal 
   [] 
   PrattParser.expression trial_tokens

(* Name the test cases and group them together *)
let suite =
"suite">:::
 [
    "basic_parser">:: basic_parser;
 ]
;;

let () =
  run_test_tt_main suite
;;
