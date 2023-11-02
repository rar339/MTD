(*Constants *)

type gamestate = Home | Inactive | Active

let lives = ref 100
let cash = ref 50000

(* Screen Constants *)
let screen_width = ref 1327.
let screen_height = ref 927.

(*Backgrounds*)

(*Current Gamestate*)
let state = ref Home

(*Utility Functions*)
let round_float x = int_of_float (Float.round x)
