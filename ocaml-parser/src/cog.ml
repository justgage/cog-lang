module Cog = struct
  open Core.Std
  open Tokenizer
  open PrattParser

  let run_ast str = 
    str
     |> Tokenizer.tokenize
     |> PrattParser.begin_parse
  ;;

    let run_fmt str = 
      str
      |> run_ast
      |> PrattParser.print
    ;;

    let run_file filename = 
      Core.In_channel.read_all filename
      |> run_ast
       (*|>  Executor.run *)
end
