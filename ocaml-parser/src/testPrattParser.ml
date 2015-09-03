open OUnit2
open PrattParser
open Tokenizer
open PrattParser
open Cog
  
(**
 * If statements in general
 *)

let basic_math _test_ctxt = 
  let got = Cog.run_string "2+2*3-4-5*9" in
  let expected = 
  "printing result:
 (((2  +  (2  *  3))  -  4)  -  (5  *  9))
" in 
  assert_equal expected got

let basic_math_whitespace _test_ctxt = 
  let got = Cog.run_string "
  2
  +
  2  * 
  3
  -4
  -5 *  9" in
  let expected = 
  "printing result:
 (((2  +  (2  *  3))  -  4)  -  (5  *  9))
" in 
  assert_equal expected got

let basic_if _test_ctxt =
  let got = Cog.run_string "if 2 then 3 else 4 end" in
  let expected = 
  "printing result:
 if 2 \n  then 3 \n  else 4 end\n" in 
  assert_equal expected got

let if_statements_gen _test_ctxt = 
  let got = 
    Cog.run_string
  "if 2 == 2 and not 4 < 2 or 2  
    then 9 + 12 + 2 + 4 + 5 + 6 + 7 + 8 
    else 2 + 2 < 3 + 2 * 8 end" 
  and expected = "printing result:
 if ((2  ==  (2  and  (not 4)))  <  (2  or  2)) 
  then (((((((9  +  12)  +  2)  +  4)  +  5)  +  6)  +  7)  +  8) 
  else ((2  +  2)  <  (3  +  (2  *  8))) end
" in
  assert_equal got expected

(* Name the test cases and group them together *)
let suite =
"suite">:::
 [
   "basic math"            >:: basic_math;
   "basic math whitespace" >:: basic_math_whitespace;
   "if basic"              >:: basic_if;
   "if general"            >:: if_statements_gen;
 ]

let () =
  run_test_tt_main suite
