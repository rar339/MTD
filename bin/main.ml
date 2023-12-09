open Raylib
open MTD
open Constants
open Gamebackground

let () = print_endline ""
let () = print_endline "*************************************"
let () = print_endline "********** Starting MTD! ************"
let () = print_endline "*************************************"
let () = print_endline ""

(*Current set of home-screen balloons*)
let balloons = ref []

let setup () =
  (*Set up the screen and fps*)
  Raylib.init_window 0 0 "MTD";
  screen_width := float_of_int (get_screen_width ());
  screen_height := float_of_int (get_screen_height ());

  toggle_fullscreen ();
  Raylib.set_target_fps 60;

  (*Set up the fonts and textures*)
  Constants.setup_fonts ();
  Constants.setup_background_imgs ();
  Constants.setup_balloon_imgs ();
  Constants.setup_bear_imgs ();
  Constants.setup_projectile_imgs ();
  Constants.setup_misc_imgs ();

  (*Set up Raygui *)
  Raygui.(set_style (Label `Text_alignment) TextAlignment.(to_int Center));
  Raygui.(set_style (Button `Base_color_normal) 0xFF000010);
  Raygui.(set_style (Button `Base_color_focused) 0x7F000000);
  Raygui.(set_style (Button `Base_color_pressed) 0x7F000000);
  Raygui.(set_style (Default `Text_color_normal) 0xFFFFFFFF);
  Raygui.set_style (Default `Text_size) 36;
  Raygui.(set_style (Button `Border_width) 5);
  Raygui.set_font (Option.get !custom_font);
  Raygui.(set_style (Label `Border_color_normal) 0xFFF);
  Raygui.(set_style (Label `Base_color_normal) 0xFF000010);
  Raygui.(set_style (Label `Base_color_focused) 0x000000);
  Raygui.(set_style (Button `Border_color_normal) 0x000000);
  Raygui.(set_style (Label `Border_color_normal) 0xFF0000);
  Raygui.(set_style (Label `Border_width) 6);
  Raygui.(set_style (Label `Text_padding) 0);
  Raygui.(set_style (Button `Text_alignment) TextAlignment.(to_int Center));
  Raygui.(set_style (Button `Base_color_focused) 0x80808080);

  balloons := generate_all_balloons 0 12 (Option.get !red_balloon_img);

  ()

(*Updates game)*)
let update_home () =
  (if is_mouse_button_pressed Left then
     let click_pos = get_mouse_position () in
     balloons := check_clicked_all_balloons !balloons click_pos);

  Gamebackground.update_balloon_positions !balloons

(*Draws home screen for MTD.*)
let draw_home () =
  begin_drawing ();
  clear_background Color.lightgray;
  (***** BACKGROUND *****)
  draw_texture_ex
    (Option.get !intro_screen_art)
    (Vector2.create 0. 0.0) (* Position *)
    0.0 (* Rotation (in radians) *)
    (!screen_width /. 1831.0) (* Scale *)
    Color.white;
  Raygui.set_font (Option.get !game_font);
  (* create -> x y width height*)
  if
    Raygui.(
      button
        (Rectangle.create
           (5. *. !screen_width /. 8.)
           (5. *. !screen_height /. 9.)
           160. 80.)
        "PLAY")
  then Constants.state := Inactive;
  draw_text_ex (Option.get !title_font) "McGraw Tower"
    (Vector2.create (!screen_width /. 2.) (!screen_height /. 3.))
    100. 3. (Color.create 255 6 0 255);
  draw_text_ex (Option.get !title_font) "Defense"
    (Vector2.create (4. *. !screen_width /. 7.) (3. *. !screen_height /. 7.))
    100. 3. (Color.create 255 6 0 255);

  (***** BALLOONS *****)
  draw_balloons !balloons;

  end_drawing ()

(*Updates and draws the window based on the current gamestate.*)
let update_and_draw tuple =
  update_home ();
  if !Constants.state <> Home then
    let open MTD in
    (* clear_background Color.lightgray; *)
    Game.loop ()
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
