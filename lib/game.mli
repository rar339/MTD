(**Contains the main loop and GUI components for the game.*)

open Balloons

val play_button : float -> float -> unit
(**The play button for the round*)

val mult_button : float -> float -> unit
(**Creates the GUI button responsible for changing the game speed.*)

(******************************************************************************)

val setup : unit -> unit
(**Sets up everything needed to run the game, such as the game window, the 
    background image, and loading the textures from image files.*)
(******************************************************************************)

val bloons_spawner : (balloon * int) list ref -> unit
(**Adds bloons that are ready to be added to the screen, to current_balloons. If
   none are ready, decreases the counter on the next balloon to be added.*)

val update_state : unit -> unit
(**If there are no balloons on the screen, the round is over. *)

(******************************************************************************)

val update_game : unit -> unit
(**Updates the state of the game before the next frame is drawn.*)

(******************************************************************************)

val draw_game : unit -> unit
(**Draws all of the game's components to the screen.*)

val loop : unit -> unit
(**This is the main game loop*)
