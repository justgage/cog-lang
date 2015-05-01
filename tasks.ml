#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;
#require "calendar";;

open Printf
(* open CalendarLib *)

(* why isn't this in the std lib? *)
let round x = int_of_float (x +. 0.5)

let relitive_date_from_hours
   hours_per_day date_start hours = 
      let module D = CalendarLib.Date in
      D.add (date_start) (D.Period.day (round (hours /. hours_per_day)));;

let relitive_date = relitive_date_from_hours 1.0;;



let main = 
   let (*  h_week   = 10 and *)
       total    = 156.0 and
       preposal = 0.05 and
       research = 0.10 and
       rec_spec = 0.20 and
       design   = 0.25 and
       pre_test = 0.10 and
       code     = 0.30 in
   let l = [
      preposal;
      research;
      rec_spec;
      design;
      pre_test;
      code;
   ] in (
      let tsum = (List.fold_left (+.) 0.0 l) in
      if tsum = 1.0 
            then printf "" 
            else  printf "ERROR sum of tasts is: %.0f leaving: %.0f points to use\n" 
            (100. *. tsum) 
            (100. *.(1.0 -. tsum));
      printf "
1. Preliminary research and proposal preparation
    - __Estimated Hours till completion:__ %.0f
    - __Start Date:__ Apr 20 
    - __Stop Date:__  Apr 30 

2. Research
    - __Estimated Hours till completion:__ %.0f
    - __Start Date:__ May 1 
    - __Stop Date:__ May 15 

3. Requirements Specification document
    - __Estimated Hours till completion:__ %.0f
    - __Start Date:__ May 15
    - __Stop Date:__  June 15

4. Design
    - __Estimated Hours till completion:__ %.0f
    - __Start Date:__ June 16
    - __Stop Date:__ July 16

5. Writing behavior driven tests.
    - __Estimated Hours till completion:__ %.0f
    - __Start Date:__ July 17
    - __Stop Date:__ July 31

6. Writing the code to preform the given behaviors previously specified.
    - __Estimated Hours till completion:__ %.0f
    - __Start Date:__ Aug 1
    - __Stop Date:__ Sep 12"
   ( preposal *. total )
   ( research *. total )
   ( rec_spec *. total )
   ( design *. total )
   ( pre_test  *. total  )
   ( code *. total )
   )



