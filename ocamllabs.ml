(* Conventions for labelling in ocamllabs/Projects repos *)

type t =
 | Event of string
 | Project of string
 | Task of string
 | Other of string

let of_string s =
  match Stringext.split ~max:2 ~on:':' (String.lowercase_ascii s) with
  |["event";x] -> Event (Stringext.trim_left x)
  |["project";x] -> Project (Stringext.trim_left x)
  |["task";x] -> Task (Stringext.trim_left x)
  |_ -> Other s

let to_string = function
  | Event s -> "Event: " ^ s
  | Project s -> "Project: " ^ s
  | Task s -> "Task: " ^ s
  | Other s -> s

