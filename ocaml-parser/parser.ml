open Core.Std
open Printf

module Colors = struct
  let normal = "\027[22m"
  let cyan str = "\027[36m" ^ str ^ normal

end

let split_up = String.split ~on:' '

(* this will eat up all space and make space around things that need it *)
let rec spacer char_list = match char_list with
  | ' ':: rest -> ' ' :: (space_eater rest) (* take one space then start eating spaces *)
  |  x :: rest -> x :: (spacer rest)
  | []         -> []
  and space_eater char_list = match char_list with
  | ' ' :: rest -> space_eater rest
  |  x  :: rest -> spacer (x :: rest)
  | []          -> []

let spacer_str str = 
  str 
  |> String.to_list 
  |> spacer 
  |> String.of_char_list

let rec print_tokens lis = match lis with
| x::xs -> printf "%s " (Colors.cyan x); print_tokens xs
| []    -> printf "\n"

let parse filename = 
   let lines = Core.In_channel.read_lines filename in
   lines
   |> List.map ~f:spacer_str
   |> List.map ~f:split_up
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
