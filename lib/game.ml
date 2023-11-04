open Raylib
open Constants
open Waves
open Bears
open Gamebackground
open Gamebounds
open Balloonpath
open Projectiles

(******************************************************************************)
let play_button screen_width screen_height =
  if
    Raygui.(
      button
        (Rectangle.create
           (149. *. screen_width /. 200.)
           (8. *. screen_height /. 9.)
           (2. *. screen_width /. 9.)
           (screen_height /. 19.))
        ("Start Round " ^ string_of_int !Constants.round))
  then (
    initialize_round waves ();
    Constants.state := Active)
(******************************************************************************)

(******************************************************************************)
let setup () =
  (*Setup backgrounds and Raygui*)
  Raygui.set_style (Default `Text_size) 30;
  (* Raygui.set_style (Default `Base_color_normal) 0x6699CC; *)
  Raygui.set_style (Default `Background_color) 0x6699CC;
  (* Raygui.set_style (Default `Border_color_normal) 0x00000; *)
  (* Raygui.set_style (Default `Border_width) 2; *)
  Raygui.(set_style (Label `Text_color_normal) 0xFFFFFFFF);

  (* Setup background image *)
  let game_image : Image.t = Raylib.load_image "./img/mtd_map.png" in
  Gamebackground.background := Some (load_texture_from_image game_image);
  background_width := Image.width game_image;
  background_height := Image.height game_image;
  (*The expression in setup_hitbox corresponds to the path_width*)
  Balloons.setup_hitbox (2. *. !screen_height /. 28.);

  (* Setup heart and cash images *)

  (*Make the menu rectangle*)
  Constants.menu_rect :=
    Some
      (Rectangle.create
         (29.5 *. floor (!screen_width /. 40.))
         (!screen_height /. 35.)
         (7. *. floor (!screen_width /. 28.))
         (38. *. !screen_height /. 40.));

  Constants.heart_img :=
    Some Raylib.(load_texture_from_image (load_image "./img/heart.png"));

  Constants.cash_img :=
    Some Raylib.(load_texture_from_image (load_image "./img/dollar.png"));

  path_rectangles := generate_rectangles screen_width screen_height;

  (*Turn points on the path*)
  turn_points := generate_turn_points screen_width screen_height;

  (*Load all the waves for the game.*)
  waves :=
    [
      Waves.test_wave screen_height;
      Waves.wave1 screen_height;
      Waves.wave2 screen_height;
    ]
(*Load initial wave, likely temporarily: just for testing*)

(******************************************************************************)
(*Adds bloons that are ready to be added to the screen, to current_bloons. If
   none are ready, decreases the counter on the next balloon to be added.*)
let bloons_spawner current_wave =
  match !current_wave with
  | [] -> ()
  | (bloon, counter) :: t when counter = 0 ->
      current_bloons := bloon :: !current_bloons;
      current_wave := t
  | (bloon, counter) :: t -> current_wave := (bloon, counter - 1) :: t

(* If there are no balloons on the screen, the round is over. *)
let update_state () =
  if !current_bloons = [] && !current_wave = [] && !Constants.state = Active
  then (
    Projectiles.bullet_collection := [];
    Constants.state := Inactive;
    Constants.cash := !Constants.cash + 100;
    Constants.round := !Constants.round + 1)

(* Checks if a bear can be placed in the right  *)
let rec check_valid_placement (mouse_pos : Vector2.t)
    (rectangle_bounds : Rectangle.t list) =
  match rectangle_bounds with
  | [] -> true
  | h :: t ->
      if check_collision_point_rec mouse_pos h == true then false
      else check_valid_placement mouse_pos t

let nevermind (mouse_pos : Vector2.t) (menu : Rectangle.t) =
  check_collision_point_rec mouse_pos menu

(* Checks for valid placement of bear, contingent on position and cash.
   If a player no longer wants to place a bear, they can move the selected
   choice back to the menu to discard their choice. *)

let check_click () =
  if is_mouse_button_pressed Left then
    if
      determine_dart_bear_clicked (get_mouse_position ()) !screen_width
        !screen_height
    then selected_bear := Some (Bears.make_dart_bear (get_mouse_position ()))
    else if
      determine_hockey_bear_clicked (get_mouse_position ()) !screen_width
        !screen_height
    then selected_bear := Some (Bears.make_hockey_bear (get_mouse_position ()))
    else if
      (determine_pumpkin_bear_clicked (get_mouse_position ()))
        !screen_width !screen_height
    then selected_bear := Some (Bears.make_pumpkin_bear (get_mouse_position ()))
    else if
      (determine_ezra_bear_clicked (get_mouse_position ()))
        !screen_width !screen_height
    then selected_bear := Some (Bears.make_ezra_bear (get_mouse_position ()))
    else if
      (determine_dragon_bear_clicked (get_mouse_position ()))
        !screen_width !screen_height
    then selected_bear := Some (Bears.make_dragon_bear (get_mouse_position ()))

let place_bear () =
  if !selected = false && !selected_bear <> None then selected := true
  else if
    nevermind (get_mouse_position ()) (Option.get !Constants.menu_rect)
    && !selected = true
    && is_mouse_button_pressed Left
  then (
    selected := false;
    selected_bear := None)
  else if
    !selected = true
    && is_mouse_button_pressed Left
    && check_valid_placement (get_mouse_position ()) !path_rectangles
    && (Option.get !selected_bear).cost <= !Constants.cash
    && Bears.check_collision_bears !selected_bear !Bears.bear_collection
       == false
  then (
    selected := false;
    Bears.bear_collection := Option.get !selected_bear :: !Bears.bear_collection;
    Constants.cash := !Constants.cash - (Option.get !selected_bear).cost;
    selected_bear := None)

(******************************************************************************)
let update_game () =
  update_state ();
  check_click ();
  place_bear ();

  Bears.update_selected_bear !selected_bear (get_mouse_position ());

  if !Constants.state = Active then (
    bloons_spawner current_wave;
    move_balloons !current_bloons !turn_points;
    fire_all_shots !bear_collection !current_bloons;
    update_bullets !bullet_collection;
    update_collisions !bullet_collection !current_bloons;
    bullet_collection := Projectiles.remove_bullets !bullet_collection;
    current_bloons := Balloons.remove_balloons !current_bloons)

(******************************************************************************)
let draw_game () =
  let open Raylib in
  begin_drawing ();
  clear_background Color.white;

  (*Draw the background & reference grid*)
  Gamebackground.draw_background background;

  (* Gamebackground.draw_ref_grid
     (int_of_float !screen_width)
     (int_of_float !screen_height); *)

  (*This line shows ref rectangles! Comment out if you want them invisible*)
  (* Gamebounds.draw_rectangles !path_rectangles; *)
  Menubar.draw_menu (Option.get !Constants.menu_rect);

  (*** Drawing lives and cash ***)
  Raylib.draw_rectangle_rec
    (Menubar.lives_box !screen_width !screen_height)
    (Color.create 150 0 0 100);

  Raylib.draw_rectangle_rec
    (Menubar.cash !screen_width !screen_height)
    (Color.create 0 150 0 100);

  Menubar.draw_heart !Constants.heart_img !screen_width !screen_height;
  Menubar.draw_cash !Constants.cash_img !screen_width !screen_height;
  Menubar.lives_and_cash_count !screen_width !screen_height;

  (* Drawing round button *)
  if !Constants.state <> Active then play_button !screen_width !screen_height;

  (*Draw the SELECTED bear to PLACE*)
  Bears.draw_selected_bear !selected_bear;

  (* Draw the menu bears *)
  Bears.draw_bear_img
    (5.45 *. !screen_width /. 7.)
    (1. *. !screen_height /. 4.)
    Color.red;

  Bears.draw_bear_img
    (5.75 *. !screen_width /. 7.)
    (1. *. !screen_height /. 4.)
    Color.blue;

  Bears.draw_bear_img
    (6.05 *. !screen_width /. 7.)
    (1. *. !screen_height /. 4.)
    Color.orange;

  Bears.draw_bear_img
    (6.35 *. !screen_width /. 7.)
    (1. *. !screen_height /. 4.)
    Color.purple;

  Bears.draw_bear_img
    (6.65 *. !screen_width /. 7.)
    (1. *. !screen_height /. 4.)
    Color.green;

  if !selected then
    if
      check_valid_placement (get_mouse_position ()) !path_rectangles
      && Bears.check_collision_bears !selected_bear !Bears.bear_collection
         == false
    then
      draw_circle
        (Constants.round_float (Vector2.x (Option.get !selected_bear).position))
        (Constants.round_float (Vector2.y (Option.get !selected_bear).position))
        (Option.get !selected_bear).range (Color.create 0 0 0 100)
    else
      draw_circle
        (Constants.round_float (Vector2.x (Option.get !selected_bear).position))
        (Constants.round_float (Vector2.y (Option.get !selected_bear).position))
        (Option.get !selected_bear).range (Color.create 100 0 0 100);

  (*Draw PLACED bears!*)
  Bears.draw_bears !Bears.bear_collection;

  (*Draw bullets*)
  draw_bullets !bullet_collection;

  (*Draw the turning points for reference, comment out if you want them invisible*)
  Balloonpath.draw_turnpoints !turn_points;

  (*Draw the balloons, the number passed in is the path's width, so that balloons
     are drawn as the correct size.*)
  if !Constants.state = Active then
    Balloons.draw_balloons (2. *. !screen_height /. 28.) !current_bloons;

  if !showInstructions then (
    draw_rectangle 0 0
      (int_of_float !screen_width)
      (int_of_float !screen_height)
      (Color.create 0 0 0 200);
    if
      let x_pos = 1. *. !screen_width /. 5. in
      let y_pos = 1. *. !screen_height /. 5. in
      let show_window =
        Raygui.window_box
          (Rectangle.create x_pos y_pos
             (3. *. !screen_width /. 5.)
             (3. *. !screen_height /. 5.))
          ""
      in

      draw_text
        "Welcome to McGraw Tower Defense!\n\n\
         Defend Cornell and McGraw Tower from waves of \n\
         oncoming balloons. You earn cash for every \n\
         layer of balloon that you pop and at the end \n\
         of each round. Use it strategically to buy and \n\
         upgrade bears. \n\n\
         Good luck!"
        (int_of_float (x_pos +. 10.))
        (int_of_float (y_pos +. 30.))
        30 Color.white;
      show_window
    then showInstructions := false);

  end_drawing ()

(*Main game loop***************************************************************)
let loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else (
    if !count = 0 then (
      count := 1;
      setup ();
      update_game ();
      draw_game ())
    else update_game ();
    draw_game ())
