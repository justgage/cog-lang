open Core.Std
open Printf
open Cog

let parse = Cog.run


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
