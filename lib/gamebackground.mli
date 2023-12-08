(**This module contains the functions that manage the background of the game, in
    both the main menu an the game itself.*)

open Raylib

val background : Texture2D.t option ref
val background_width : int ref
val background_height : int ref

val draw_background : Texture2D.t -> unit
(**Draws the background of the game.*)

val draw_ref_grid : int -> int -> unit
(**Draws reference rectangles*)

type balloon = {
  x : float;
  mutable y : float;
  speed : float;
  img : Texture2D.t;
}

val gen_balloon : float -> float -> float -> Texture2D.t -> balloon
(**Generates a balloon to go on the menu screen.*)

val generate_all_balloons : int -> int -> Texture2D.t -> balloon list
(**Generates all the balloons we want to display on the menu screen.*)

val update_balloon_position : balloon -> unit
(**Updates the position of a menu screen balloon.*)

val update_balloon_positions : balloon list -> unit
(**Updates the positions of all the menu screen balloons.*)

val draw_balloon : balloon -> unit
(**Draws a balloon on the menu screen.*)

val draw_balloons : balloon list -> unit
(**Draws all the balloons in a balloon list to the menu screens.*)

val check_clicked : balloon -> Vector2.t -> bool
(**Check if a balloon was clicked.*)

val check_clicked_all_balloons : balloon list -> Vector2.t -> balloon list
(**If a balloon in a balloon list is clicked, removes that balloon from the screen.*)
