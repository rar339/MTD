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

  (*Art*)
  let title_font = ref None
  let background = ref None
  let red_bal_texture = ref None
end

(*Home screen baloons**********************************************************)
module Baloon = struct
  type baloon = {
    x : float;
    y : float;
    speed : float;
    bal_texture : Texture2D.t;
  }

  let gen_baloon x y speed bal_texture = { x; y; speed; bal_texture }

  let update_baloon_position baloonRef =
    let baloon = !baloonRef in
    let new_y = if baloon.y < ~-.5. then 650. else baloon.y -. baloon.speed in
    baloonRef :=
      {
        x = baloon.x;
        y = new_y;
        speed = baloon.speed;
        bal_texture = baloon.bal_texture;
      }

  let rec update_baloon_positions (baloons : baloon ref list) =
    match baloons with
    | [] -> ()
    | h :: t ->
        update_baloon_position h;
        update_baloon_positions t

  let draw_baloon { x; y; speed = _; bal_texture } =
    draw_texture_ex bal_texture (Vector2.create x y) 0.0 0.15 Color.white

  let rec draw_baloons (baloons : baloon ref list) =
    match baloons with
    | [] -> ()
    | h :: t ->
        draw_baloon !h;
        draw_baloons t
end

let gui_setup () =
  let title_fon = Raylib.load_font_ex "machine-gunk.ttf" 100 None in
  let custom_font = Raylib.load_font_ex "machine-gunk.ttf" 24 None in
  Raygui.set_font custom_font;
  (*Create the intro screen art*)
  let intro_screen_art = Raylib.load_image "MTDCoverArt.png" in
  let back = Raylib.load_texture_from_image intro_screen_art in
  unload_image intro_screen_art;

  let red_balloon = Raylib.load_image "red.png" in
  let red_bal = Raylib.load_texture_from_image red_balloon in
  unload_image red_balloon;

  Constants.background := Some back;
  Constants.red_bal_texture := Some red_bal;
  Constants.title_font := Some title_fon

(*Current set of baloons*)
let baloons = ref []

let setup () =
  Raylib.init_window Constants.screen_width Constants.screen_height "MTD";
  Raylib.set_target_fps 60;

  (*Create the intro screen art*)
  gui_setup ();

  (*Generate Baloons*)
  baloons :=
    ref
      (Baloon.gen_baloon 500.0 550.0 5.0
         (Option.get !Constants.red_bal_texture))
    :: !baloons

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
  if Raygui.(button (Rectangle.create 550. 360. 120. 50.) "PLAY") then
    Constants.current_gamestate := Active;

  (***** BALLOONS *****)
  Baloon.update_baloon_positions !baloons;
  Baloon.draw_baloons !baloons;

  end_drawing ()

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
