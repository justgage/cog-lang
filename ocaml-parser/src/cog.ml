module Cog = struct
  open Core.Std
  open Tokenizer
  open PrattParser
  open Eval

  let run_ast str =
    str
     |> Tokenizer.tokenize
     |> PrattParser.begin_parse

    let run_fmt_string str =
      str
      |> run_ast
      |> PrattParser.to_string

    let run_fmt str =
      str
      |> run_ast
      |> PrattParser.print

    let run str =
      let ast_result = run_ast str in
      match ast_result with
      | Error _ as x -> printf "Parser problem:\n%s" (PrattParser.to_string x)
      | Ok ps ->
        PrattParser.(ps.parsed)
        |> Eval.eval
        |> Eval.print_op

    let run_str str : string =
      let ast_result = run_ast str in
      match ast_result with
      | Error _ as err-> PrattParser.to_string err
      | Ok ps ->
        (PrattParser.(ps.parsed)
            |> Eval.eval
            |> Eval.to_string)

    let run_value str =
      let ast_result = run_ast str in
      match ast_result with
      | Error _ as x -> x
      | Ok ps ->
        Ok (PrattParser.(ps.parsed)
            |> Eval.eval)

    let run_file filename =
      Core.In_channel.read_all filename
      |> run
end
