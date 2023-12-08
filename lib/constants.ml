open Raylib

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
let cash = ref 0

(* If a baloon is ever at a negative y value, it has reached the end of the path. *)
let end_line = 0.0

(* Screen constants *)
let screen_width = ref 1194.
let screen_height = ref 834.

(* Current gamestate *)
let state = ref Home
let round = ref 1

(* Game speed multiplier *)
let speed_mult = ref 1

(*Game restart attributes*)
let start_cash = 40000
let start_lives = 50

(*Game global attributes*)
let count = ref 0
let showInstructions = ref true
let restartGame = ref false
let selected : bool ref = ref false
let menu_rect : Raylib.Rectangle.t option ref = ref None
let mult_rect : Raylib.Rectangle.t option ref = ref None
let heart_img : Raylib.Texture2D.t option ref = ref None
let cash_img : Raylib.Texture2D.t option ref = ref None
let pop_img : Raylib.Texture2D.t option ref = ref None
let dart_img : Raylib.Texture2D.t option ref = ref None
let selection_rect : Raylib.Rectangle.t option ref = ref None

(******************************************************************************)
(*Set up fonts*)
let game_font : Raylib.Font.t option ref = ref None
let title_font : Raylib.Font.t option ref = ref None
let custom_font : Raylib.Font.t option ref = ref None
let menu_font : Raylib.Font.t option ref = ref None

let setup_fonts () =
  game_font :=
    Some
      (Raylib.load_font_ex "./img/fonts/gamefont.otf"
         (Raylib.get_screen_width () / 30)
         None);
  title_font :=
    Some (Raylib.load_font_ex "./img/fonts/machine-gunk.ttf" 100 None);
  menu_font :=
    Some
      (Raylib.load_font_ex "./img/fonts/gamefont.otf"
         (Raylib.get_screen_width () / 50)
         None);

  custom_font :=
    Some
      (Raylib.load_font_ex "./img/fonts/machine-gunk.ttf"
         (Raylib.get_screen_width () / 50)
         None)

(******************************************************************************)
(*Set up the title screen art*)
let intro_screen_art : Raylib.Texture2D.t option ref = ref None
let game_background_img : Raylib.Texture2D.t option ref = ref None

let setup_background_imgs () =
  intro_screen_art :=
    Some (Raylib.load_texture "./img/background/MTDCoverArt.png");
  game_background_img :=
    Some (Raylib.load_texture "./img/background/mtd_map.png")

(******************************************************************************)

(* Textures for projectiles*)
let hockeypuck_img : Texture2D.t option ref = ref None
let fireball_img : Texture2D.t option ref = ref None
let dartshot_img : Texture2D.t option ref = ref None

(*Textures for in-game bears*)
let dartbear_img : Texture2D.t option ref = ref None
let hockeybear_img : Texture2D.t option ref = ref None
let polarbear_img : Texture2D.t option ref = ref None
let sniperbear_img : Texture2D.t option ref = ref None
let dragonbear_img : Texture2D.t option ref = ref None

(*Textures for menu bears*)
let menu_dartbear_img : Texture2D.t option ref = ref None
let menu_hockeybear_img : Texture2D.t option ref = ref None
let menu_polarbear_img : Texture2D.t option ref = ref None
let menu_sniperbear_img : Texture2D.t option ref = ref None
let menu_dragonbear_img : Texture2D.t option ref = ref None

(*Textures for balloons*)
let red_balloon_img : Texture2D.t option ref = ref None
let blue_balloon_img : Texture2D.t option ref = ref None
let green_balloon_img : Texture2D.t option ref = ref None
let yellow_balloon_img : Texture2D.t option ref = ref None
let orange_balloon_img : Texture2D.t option ref = ref None
let purple_balloon_img : Texture2D.t option ref = ref None
let lead_balloon_img : Texture2D.t option ref = ref None

(******************************************************************************)
(*Set up textures*)
let setup_bear_imgs () =
  (*in-game bears*)
  dartbear_img := Some (Raylib.load_texture "./img/bears/dartbear.png");
  hockeybear_img := Some (Raylib.load_texture "./img/bears/hockeybear.png");
  polarbear_img := Some (Raylib.load_texture "./img/bears/polarbear.png");
  sniperbear_img := Some (Raylib.load_texture "./img/bears/sniperbear.png");
  dragonbear_img := Some (Raylib.load_texture "./img/bears/redbear.png");
  (*menu bears*)
  menu_dartbear_img :=
    Some (Raylib.load_texture "./img/bears/menu_dartbear.png");
  menu_hockeybear_img :=
    Some (Raylib.load_texture "./img/bears/menu_hockeybear.png");
  menu_polarbear_img :=
    Some (Raylib.load_texture "./img/bears/menu_polarbear.png");
  menu_sniperbear_img :=
    Some (Raylib.load_texture "./img/bears/menu_sniperbear.png");
  menu_dragonbear_img := Some (Raylib.load_texture "./img/bears/redbear.png")

(*Sets up balloon textures*)
let setup_balloon_imgs () =
  red_balloon_img := Some (Raylib.load_texture "./img/balloons/red.png");
  blue_balloon_img := Some (Raylib.load_texture "./img/balloons/blue.png");
  green_balloon_img := Some (Raylib.load_texture "./img/balloons/green.png");
  yellow_balloon_img := Some (Raylib.load_texture "./img/balloons/yellow.png");
  orange_balloon_img := Some (Raylib.load_texture "./img/balloons/orange.png");
  purple_balloon_img := Some (Raylib.load_texture "./img/balloons/purple.png");
  lead_balloon_img := Some (Raylib.load_texture "./img/balloons/lead.png")

let setup_projectile_imgs () =
  hockeypuck_img :=
    Some (Raylib.load_texture "./img/projectiles/hockeypuck.png");
  fireball_img := Some (Raylib.load_texture "./img/projectiles/fireball.png");
  dartshot_img := Some (Raylib.load_texture "./img/projectiles/dartshot.png")

(******************************************************************************)
(*Setup misc images*)
let dollar_img : Texture2D.t option ref = ref None
let heart_logo_img : Texture2D.t option ref = ref None
let popped_img : Texture2D.t option ref = ref None

let setup_misc_imgs () =
  heart_logo_img := Some (Raylib.load_texture "./img/misc/heart_logo.png");
  dollar_img := Some (Raylib.load_texture "./img/misc/dollar.png");
  pop_img := Some (Raylib.load_texture "./img/misc/popped.png")

(* Utility Functions *)
let round_float x = int_of_float (Float.round x)

let list_from_yojson (dimensions_list : Yojson.Basic.t) =
  match dimensions_list with
  | `List dims -> List.map Yojson.Basic.Util.to_float dims
  | _ -> failwith "impossible"
