(*Constants********************************************************************)
(*Custom Types*)
type gamestate = Home | Active

(*Screen Constants*)
let screen_width = ref 1100.0
let screen_height = ref 720.0

(*Backgrounds*)

(*Current Gamestate*)
let state = ref Home

(*Utility Functions*)
let round_float x = int_of_float (Float.round x)
