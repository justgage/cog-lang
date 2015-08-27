(* broken for some reason -_- *)
  open Tokenizer
  open PrattParser
  open Core.Std
  open OUnit2

  let trial_tokens () = 
    List.map ~f:Tokenizer.tokenize [
      "2" ;
      "2+3" ;
      "2+3*4" ;
      "4*3*4" ;
      "4*3+4" ;
    ]

   
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
