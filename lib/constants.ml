(* This is a file containing the relevant constants for our game. Those
   that are mutable are declared as pointers. *)
type gamestate = Home | Inactive | Active

(* The number of lives a player has in the game. When a balloon leaves through
   the finish life, the player loses lives associated with the balloon's 
   value. *)
let lives = ref 100

(* The player's spending money for bears and their upgrades. Cash is 
   spent with every purchase and accrued with every layer of balloon popped
   or level completed. *)
let cash = ref 500

(* If a baloon is ever at a negative y value, it has reached the end of the path. *)
let end_line = -60.

(* Screen constants *)
let screen_width = ref 1194.
let screen_height = ref 834.

(* Current gamestate *)
let state = ref Home

(* Utility Functions *)
let round_float x = int_of_float (Float.round x)
