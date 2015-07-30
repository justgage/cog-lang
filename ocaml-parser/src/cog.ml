module Cog = struct
  open Core.Std
  open Tokenizer
  open Parser

    let run_ast filename = 
      Core.In_channel.read_all filename
       |> Tokenizer.tokenize
       |> Parser.parse

    let run filename = 
      Core.In_channel.read_all filename
       |> Tokenizer.tokenize
       |> Parser.parse
       |> Parser.print_tree
end
