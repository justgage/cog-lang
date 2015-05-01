#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;
#require "calendar";;

open Printf;;

let module P = CalendarLib.Printer.Calendar in
let module Cal = CalendarLib.Calendar in (
   printf "Task Name: ";
   flush stdout;
   let name = (input_line stdin)
   and now = Cal.now () in (
      printf "Starting: %s at: " name;
      P.dprint now;
      printf "\nPush enter to end timer.";
      flush stdout;
      let ????
      )
)


