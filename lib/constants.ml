(* This is a file containing the relevant constants for our game. Those
   that are mutable are declared as pointers. *)
type gamestate = Home | Inactive | Active

(* The number of lives *)
let lives = ref 100
let cash = ref 500

(*If a baloon is ever at a negative y value, it has reached the end of the path.*)
let end_line = -60.

(* Screen Constants *)
let screen_width = ref 1194.
let screen_height = ref 834.

(*Backgrounds*)

(*Current Gamestate*)
let state = ref Home

(*Utility Functions*)
let round_float x = int_of_float (Float.round x)
