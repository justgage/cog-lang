open Core.Std
open Printf
open Cog

let spec =
  let open Command.Spec in
  empty
  +> flag "-f" (required string) ~doc: "file to parse"

let command =
  Command.basic
  ~summary: "The Cog programing language interpeter"
  ~readme:(fun () -> "More detailed info")
  spec
  (fun filename () -> Cog.run_file filename; ())

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command
