(** This is a file containing the relevant constants for our game. Those
   that are mutable are declared as references. *)

open Raylib

type gamestate = Home | Inactive | Active | Lose

val lives : int ref
(** The number of lives a player has in the game. When a balloon leaves through
  the finish life, the player loses lives associated with the balloon's
  value. *)

val cash : int ref
(** The player's spending money for bears and their upgrades. Cash is
  spent with every purchase and accrued with every layer of balloon popped
  or level completed. *)

(* If a baloon is ever at this y value, it has reached the end of the path. *)
val end_line : float

(* Screen constants *)
val screen_width : float ref
val screen_height : float ref

(* Current gamestate *)
val state : gamestate ref
val round : int ref

(* Game speed multiplier *)
val speed_mult : int ref

(*Game restart attributes*)
val start_cash : int
val start_lives : int

(*Game global attributes*)
val count : int ref
val showInstructions : bool ref
val restartGame : bool ref
val selected : bool ref
val menu_rect : Raylib.Rectangle.t option ref
val mult_rect : Raylib.Rectangle.t option ref
val heart_img : Raylib.Texture2D.t option ref
val cash_img : Raylib.Texture2D.t option ref
val pop_img : Raylib.Texture2D.t option ref
val dart_img : Raylib.Texture2D.t option ref
val selection_rect : Raylib.Rectangle.t option ref

(*Text fonts*)
val game_font : Raylib.Font.t option ref
val title_font : Raylib.Font.t option ref
val custom_font : Raylib.Font.t option ref
val menu_font : Raylib.Font.t option ref

(*Set up the title screen art*)
val intro_screen_art : Raylib.Texture2D.t option ref
val game_background_img : Raylib.Texture2D.t option ref

(* Textures for projectiles*)
val hockeypuck_img : Texture2D.t option ref
val fireball_img : Texture2D.t option ref
val dartshot_img : Texture2D.t option ref

(*Textures for in-game bears*)
val dartbear_img : Texture2D.t option ref
val hockeybear_img : Texture2D.t option ref
val polarbear_img : Texture2D.t option ref
val sniperbear_img : Texture2D.t option ref
val dragonbear_img : Texture2D.t option ref

(*Textures for menu bears*)
val menu_dartbear_img : Texture2D.t option ref
val menu_hockeybear_img : Texture2D.t option ref
val menu_polarbear_img : Texture2D.t option ref
val menu_sniperbear_img : Texture2D.t option ref
val menu_dragonbear_img : Texture2D.t option ref

(*Textures for balloons*)
val red_balloon_img : Texture2D.t option ref
val blue_balloon_img : Texture2D.t option ref
val green_balloon_img : Texture2D.t option ref
val yellow_balloon_img : Texture2D.t option ref
val orange_balloon_img : Texture2D.t option ref
val purple_balloon_img : Texture2D.t option ref
val lead_balloon_img : Texture2D.t option ref

(*Misc images*)
val dollar_img : Texture2D.t option ref
val heart_logo_img : Texture2D.t option ref
val popped_img : Texture2D.t option ref

val setup_fonts : unit -> unit
(**Loads the textures for the fonts.*)

val setup_background_imgs : unit -> unit
(**Loads the textures for the backgrounds.*)

val setup_bear_imgs : unit -> unit
(**Loads the textures for the bears.*)

val setup_balloon_imgs : unit -> unit
(**Loads the textures for the balloons.*)

val setup_projectile_imgs : unit -> unit
(**Loads the textures for the projectiles.*)

val setup_misc_imgs : unit -> unit
(**Loads the textures for miscellaneous.*)

val list_from_yojson : Yojson.Basic.t -> float list
(**Parses a yojson into a ocaml float list.*)

val round_float : float -> int
(**Rounds floats to an int. E.g. 5.6 -> 6 and 5.3 -> 5.*)
