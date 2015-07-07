open Core.Std
open Printf

module Colors = struct
  let normal = "\027[22m"
  let cyan str = "\027[36m" ^ str ^ normal

end

let split_up = String.split ~on:' '

let rec print_tokens lis = match lis with
| x::xs -> printf "%s " (Colors.cyan x); print_tokens xs
| []    -> printf "\n"

let parse filename = 
   let lines = Core.In_channel.read_lines filename in
   lines
   (* |> List.map ~f:spacer_str *)
   |> List.map ~f:split_up
   |> List.map ~f:Tokenize.from_str_list
   |> List.iter ~f:(print_tokens);
   ();;


let spec =
  let open Command.Spec in
  empty
  +> flag "-f" (required string) ~doc: "file to parse"

let command = 
  Command.basic
  ~summary: "Add a jrnl entry :)"
  ~readme:(fun () -> "More detailed info")
  spec
  (fun filename () -> parse filename)

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command