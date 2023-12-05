(* This is a file containing the relevant constants for our game. Those
   that are mutable are declared as pointers. *)
type gamestate = Home | Inactive | Active | Lose

(* The number of lives a player has in the game. When a balloon leaves through
   the finish life, the player loses lives associated with the balloon's
   value. *)
let lives = ref 100

(* The player's spending money for bears and their upgrades. Cash is
   spent with every purchase and accrued with every layer of balloon popped
   or level completed. *)
let cash = ref 5000

(* If a baloon is ever at a negative y value, it has reached the end of the path. *)
let end_line = -60.

(* Screen constants *)
let screen_width = ref 1194.
let screen_height = ref 834.

(* Current gamestate *)
let state = ref Home
let round = ref 1

(*Game restart attributes*)
let start_cash = 4000
let start_lives = 150

(*Game global attributes*)
let count = ref 0
let showInstructions = ref true
let restartGame = ref false
let game_font : Raylib.Font.t option ref = ref None
let selected : bool ref = ref false
let selected_bear : Bears.bear option ref = ref None
let menu_rect : Raylib.Rectangle.t option ref = ref None
let heart_img : Raylib.Texture2D.t option ref = ref None
let cash_img : Raylib.Texture2D.t option ref = ref None
let pop_img : Raylib.Texture2D.t option ref = ref None
let dart_img : Raylib.Texture2D.t option ref = ref None
let selection_rect : Raylib.Rectangle.t option ref = ref None

(* Utility Functions *)
let round_float x = int_of_float (Float.round x)

let list_from_yojson (dimensions_list : Yojson.Basic.t) =
  match dimensions_list with
  | `List dims -> List.map Yojson.Basic.Util.to_float dims
  | _ -> failwith "impossible"
