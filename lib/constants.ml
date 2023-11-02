(*Constants *)

type gamestate = Home | Inactive | Active

let lives = ref 100
let cash = ref 500

(* Screen Constants *)
let screen_width = ref 1194.
let screen_height = ref 834.

(*Backgrounds*)

(*Current Gamestate*)
let state = ref Home

(*Utility Functions*)
let round_float x = int_of_float (Float.round x)
