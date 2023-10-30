open Raylib
open MTD
open Constants

let () = print_endline ""
let () = print_endline "*************************************"
let () = print_endline "********** Starting MTD! ************"
let () = print_endline "*************************************"
let () = print_endline ""

(*Home screen balloons**********************************************************)
module Balloon = struct
  let balloon_h_radius = 20.
  let balloon_v_radius = 25.

  type balloon = { x : float; mutable y : float; speed : float }

  let gen_balloon x y speed = { x; y; speed }

  let rec generate_all_balloons x_pos_start count : balloon list =
    if x_pos_start < !Constants.screen_width - 100 then
      let rand_x = Random.float 300. in
      let rand_y = Random.int 200 in
      let rand_speed = Random.float 1. in
      gen_balloon
        (float_of_int x_pos_start +. rand_x)
        (float_of_int (!Constants.screen_height + rand_y))
        (rand_speed +. 1.)
      :: generate_all_balloons (x_pos_start + int_of_float rand_x) (count - 1)
    else []

  let update_balloon_position balloon =
    let new_y =
      if balloon.y < -70. then 650. +. 70. else balloon.y -. balloon.speed
    in
    balloon.y <- new_y

  let rec update_balloon_positions (balloons : balloon list) =
    match balloons with
    | [] -> ()
    | h :: t ->
        update_balloon_position h;
        update_balloon_positions t

  let draw_balloon (texture : Texture2D.t) balloon =
    draw_texture_ex texture
      (Vector2.create balloon.x balloon.y)
      0.0 0.15 Color.white

  let rec draw_balloons (texture : Texture2D.t) (balloons : balloon list) =
    match balloons with
    | [] -> ()
    | h :: t ->
        draw_balloon texture h;
        draw_balloons texture t

  let check_clicked balloon (click_pos : Vector2.t) : bool =
    (*Magic numbers represent the offset from the top left corner of png to
       actual center of balloon ellipse.*)
    let bal_x = balloon.x +. 29.0 in
    let bal_y = balloon.y +. 35.0 in
    if
      Vector2.x click_pos < bal_x +. balloon_h_radius
      && Vector2.x click_pos > bal_x -. balloon_h_radius
      && Vector2.y click_pos < bal_y +. balloon_v_radius
      && Vector2.y click_pos > bal_y -. balloon_v_radius
    then true
    else false

  let rec check_clicked_all_balloons balloons (click_pos : Vector2.t) :
      balloon list =
    match balloons with
    | [] -> []
    | balloon :: rest ->
        if check_clicked balloon click_pos then
          check_clicked_all_balloons rest click_pos
        else balloon :: check_clicked_all_balloons rest click_pos
end

(*Current set of balloons*)
let balloons = ref []

let setup () =
  Raylib.init_window 0 0 "MTD";
  screen_width := get_screen_width ();
  screen_height := get_screen_height ();
  toggle_fullscreen ();
  Raylib.set_target_fps 60;

  (*Create the intro screen art*)
  let title_font = Raylib.load_font_ex "machine-gunk.ttf" 100 None in
  let custom_font = Raylib.load_font_ex "machine-gunk.ttf" 24 None in
  Raygui.set_font custom_font;
  (*Create the intro screen art*)
  let intro_screen_art = Raylib.load_image "MTDCoverArt.png" in
  let background = Raylib.load_texture_from_image intro_screen_art in
  unload_image intro_screen_art;
  let red_balloon = Raylib.load_image "red.png" in
  let red_bal_texture = Raylib.load_texture_from_image red_balloon in
  unload_image red_balloon;
  Raygui.(set_style (TextBox `Text_alignment) TextAlignment.(to_int Center));
  (* SETTING STYLE TO RED - USE HEX*)
  Raygui.(set_style (Button `Base_color_normal) 0xFF000010);
  Raygui.(set_style (Button `Text_color_normal) 0xFFFFFF);
  Raygui.set_style (Default `Text_size) 24;

  Raygui.(set_style (Button `Border_width) 0);

  balloons := Balloon.generate_all_balloons 0 12;

  (title_font, background, red_bal_texture)

(*Updates game)*)
let update_home () =
  (if is_mouse_button_pressed Left then
     let click_pos = get_mouse_position () in
     balloons := Balloon.check_clicked_all_balloons !balloons click_pos);

  Balloon.update_balloon_positions !balloons

(*Draws home screen for MTD.*)
let draw_home (title_font, background, red_bal_texture) =
  begin_drawing ();
  clear_background Color.lightgray;
  (***** BACKGROUND *****)
  draw_texture_ex background
    (Vector2.create 0. 0.0) (* Position *)
    0.0 (* Rotation (in radians) *)
    (float_of_int !screen_width /. 1831.0) (* Scale *)
    Color.white;

  (* create -> x y width height*)
  if Raygui.(button (Rectangle.create 660. 370. 120. 50.) "PLAY") then
    Constants.state := Active;
  (* Raylib.set_texture_filter (Font.texture (Raylib.get_font_default ())) TextureFilter.Point; *)
  draw_text_ex title_font "McGraw Tower" (Vector2.create 430. 140.) 100. 3.
    (Color.create 255 6 0 255);
  draw_text_ex title_font "Defense" (Vector2.create 570. 250.) 100. 3.
    (Color.create 255 6 0 255);

  (***** BALLOONS *****)
  Balloon.draw_balloons red_bal_texture !balloons;
  
  end_drawing ()

(*Updates and draws the window based on the current gamestate.*)
let update_and_draw tuple =
  update_home ();
  if !Constants.state = Active then
    let open MTD in
    (* clear_background Color.lightgray; *)
    Game.loop()
  else draw_home tuple

(*This is the main game loop. This is the loop that is recursively called every
   tick to generate the next frame. Depending on the gamestate, a different
   function is chosen to update then draw the game.*)
let rec loop tuple =
  if Raylib.window_should_close () then Raylib.close_window ()
  else (
    update_and_draw tuple;
    loop tuple)

let () = setup () |> loop
