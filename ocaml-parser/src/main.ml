open Core.Std
open Printf

let parse filename = 
   Core.In_channel.read_lines filename
   |> List.map ~f:Tokenizer.tokenize
   (* |> List.iter ~f:Tokenizer.print_tokens *)
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
