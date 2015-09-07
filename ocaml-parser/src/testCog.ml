open OUnit2
open PrattParser
open Tokenizer
open PrattParser
open Cog


let basic_if _test_ctxt =
  let got = Cog.run_value "if true then 1 else 9 end" in
  let expected =  in
  assert_equal expected got

(* Name the test cases and group them together *)
let suite =
"suite">:::
 [
   "basic math"            >:: basic_math;
   "basic math whitespace" >:: basic_math_whitespace;
   "if basic"              >:: basic_if;
   "if general"            >:: if_statements_gen;
   "assignment + if"       >:: assignment_if;
 ]

let () =
  run_test_tt_main suite
