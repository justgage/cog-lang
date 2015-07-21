open Core.Std
open Printf

let parse filename = 
   Core.In_channel.read_all filename
   |> Tokenizer.tokenize
   |> Tokenizer.print_tokens_debug
   |> Parser.parse
   |> Parser.print_tree
   


let spec =
  let open Command.Spec in
  empty
  +> flag "-f" (required string) ~doc: "file to parse"

let command = 
  Command.basic
  ~summary: "Add a jrnl entry :)"
  ~readme:(fun () -> "More detailed info")
  spec
  (fun filename () -> parse filename; ())

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command
