open Raylib

let () = print_endline ""
let () = print_endline "*************************************"
let () = print_endline "********** Starting MTD! ************"
let () = print_endline "*************************************"
let () = print_endline ""

(*Constants********************************************************************)
module Constants = struct
  (*Custom Types*)
  type gamestate = Home | Active

  (*Screen Constants*)
  let screen_width = 1100
  let screen_height = 720

  (*Current Gamestate*)
  let current_gamestate = ref Home

  (*Art. These have to be options because they are not initially set when main
     runs*)
  let title_font = ref None
  let background = ref None
  let red_bal_texture = ref None
end

(*Home screen balloons**********************************************************)
module Baloon = struct
  type baloon = {
    x : float;
    y : float;
    speed : float;
    bal_texture : Texture2D.t;
  }

  let gen_baloon x y speed bal_texture = { x; y; speed; bal_texture }

  let rec generate_all_baloons x_pos_start count  : baloon list =
    if x_pos_start < Constants.screen_width-100 then
      let rand_x = Random.float 300. in 
      let rand_y = Random.int 200 in
      let rand_speed = Random.float 2. in
      gen_baloon
        (float_of_int x_pos_start +. rand_x)
        (float_of_int (Constants.screen_height + rand_y))  rand_speed
        (Option.get !Constants.red_bal_texture)
      :: generate_all_baloons (x_pos_start + (int_of_float rand_x)) (count - 1)
    else []

  let update_baloon_position baloon =
    let new_y = if baloon.y < -70. then 650. +. 70. else baloon.y -. baloon.speed in
    { baloon with y = new_y }

  (* let update_baloon_position baloonRef =
     let baloon = !baloonRef in
     let new_y = if baloon.y < ~-.5. then 650. else baloon.y -. baloon.speed in
     baloonRef :=
       {
         x = baloon.x;
         y = new_y;
         speed = baloon.speed;
         bal_texture = baloon.bal_texture;
       } *)

  let rec update_baloon_positions (baloons : baloon list) =
    match baloons with
    | [] -> []
    | h :: t -> update_baloon_position h :: update_baloon_positions t

  (* let rec update_baloon_positions (baloons : baloon list) =
     match balloons with
     | [] -> ()
     | h :: t ->
         update_balloon_position h;
         update_balloon_positions t *)

  let draw_baloon { x; y; speed = _; bal_texture } =
    draw_texture_ex bal_texture (Vector2.create x y) 0.0 0.15 Color.white

  let rec draw_baloons (baloons : baloon list) =
    match baloons with
    | [] -> ()
    | h :: t ->
        draw_baloon h;
        draw_baloons t
end


open Constants

(*Loads images and fonts for use on the home screen. This function sets the global
   constants background, red_bal_texture, and title_font.*)
let gui_setup () =
  let the_title_font = Raylib.load_font_ex "machine-gunk.ttf" 100 None in
  let custom_font = Raylib.load_font_ex "machine-gunk.ttf" 24 None in
  Raygui.set_font custom_font;
  (*Create the intro screen art*)
  let intro_screen_art = Raylib.load_image "MTDCoverArt.png" in
  let back = Raylib.load_texture_from_image intro_screen_art in
  unload_image intro_screen_art;

  let red_baloon = Raylib.load_image "red.png" in
  let red_bal = Raylib.load_texture_from_image red_baloon in
  unload_image red_baloon;

  Constants.background := Some back;
  Constants.red_bal_texture := Some red_bal;
  Constants.title_font := Some the_title_font

(*Current set of balloons*)
let baloons = ref []

let setup () =
  Raylib.init_window Constants.screen_width Constants.screen_height "MTD";
  Raylib.set_target_fps 60;

  (*Create the intro screen art*)
  gui_setup ();

  (*Generate Baloons*)
  (* baloons :=
     ref
       (Baloon.gen_baloon 500.0 550.0 5.0
          (Option.get !Constants.red_bal_texture))
     :: !baloons *)
  baloons := Baloon.generate_all_baloons 0 12

(*Updates and draws the initial home screen for MTD.*)
let draw_home () =
  begin_drawing ();
  (***** BACKGROUND *****)
  draw_texture_ex
    (Option.get !Constants.background)
    (Vector2.create 0. 0.0) (* Position *)
    0.0 (* Rotation (in radians) *)
    0.60 (* Scale *)
    Color.white;

  (* Raylib.set_texture_filter (Font.texture (Raylib.get_font_default ())) TextureFilter.Point; *)
  draw_text_ex
    (Option.get !Constants.title_font)
    "McGraw Tower" (Vector2.create 430. 140.) 100. 3. (Color.create 255 6 0 255);
  draw_text_ex
    (Option.get !Constants.title_font)
    "Defense" (Vector2.create 570. 250.) 100. 3. (Color.create 255 6 0 255);

  Raygui.(set_style (TextBox `Text_alignment) TextAlignment.(to_int Center));
  (* SETTING STYLE TO RED - USE HEX*)
  Raygui.(set_style (Button `Base_color_normal) 0xFF000010);
  Raygui.(set_style (Button `Text_color_normal) 0xFFFFFF);
  Raygui.set_style (Default `Text_size) 24;

  Raygui.(set_style (Button `Border_width) 0);
  (* create -> x y width height*)
  if Raygui.(button (Rectangle.create 660. 370. 120. 50.) "PLAY") then
    Constants.current_gamestate := Active;

  (***** BALLOONS *****)
  baloons := Baloon.update_baloon_positions !baloons;
  Baloon.draw_baloons !baloons;

  end_drawing ()

(*This is the main game loop. This is the loop that is recursively called every
   tick to generate the next frame. Depending on the gamestate, a different
   function is chosen to update then draw the game.*)
let rec loop update_draw_func =
  if Raylib.window_should_close () then Raylib.close_window ()
  else if !Constants.current_gamestate = Active then (
    let open MTD in
    update_draw_func ();
    loop Dragdrop.loop)
  else update_draw_func ();
  loop update_draw_func

let () =
  setup ();
  loop draw_home
