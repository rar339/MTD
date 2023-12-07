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
           (155. *. screen_width /. 200.)
           (8. *. screen_height /. 9.)
           (1.5 *. screen_width /. 9.)
           (screen_height /. 14.))
        ("Start Round " ^ string_of_int !Constants.round))
  then (
    initialize_round waves;
    Constants.state := Active)

(******************************************************************************)
let mult_button screen_width screen_height =
  if
    Raygui.(
      button
        (Rectangle.create
           (155. *. screen_width /. 200.)
           (7.25 *. screen_height /. 9.)
           (1.5 *. screen_width /. 9.)
           (screen_height /. 19.))
        (string_of_int !Constants.speed_mult ^ "X Speed"))
  then (
    if !Constants.speed_mult = 1 then Constants.speed_mult := 2
    else Constants.speed_mult := 1;
    Waves.update_wave_speeds !current_wave;
    Waves.update_balloon_speeds !current_balloons;
    Bears.update_bear_firing_rate !bear_collection)

(* draw_rectangle_rec (Rectangle.create
   (155. *. screen_width /. 200.)
   (7.25 *. screen_height /. 9.)
   (1.5 *. screen_width /. 9.)
   (screen_height /. 19.))
   Color.green;
   draw_text_ex (Option.get !custom_font) (string_of_int !Constants.speed_mult ^ "X Speed")
   (Vector2.create (155. *. screen_width /. 200.)
   (7.25 *. screen_height /. 9.)) 36. 10. (Color.create 0 0 0 255) *)

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
  background_width := Texture.width (Option.get !game_background_img);
  background_height := Texture.height (Option.get !game_background_img);

  (*The expression in setup_hitbox corresponds to the path_width*)
  Balloons.setup_hitbox (2. *. !screen_height /. 28.);

  (*Setup ALL game images into refs*)
  (* Balloons.setup_balloon_imgs ();
     Bears.setup_bear_imgs (); *)

  (*Adds the bears that will be displayed to menu_bears.*)
  Bears.menu_bears := generate_menu_bears !screen_width !screen_height;

  (*Set the initial state*)
  Bears.bear_collection := [];
  current_balloons := [];
  current_wave := [];
  lives := start_lives;
  cash := start_cash;

  (*Make the menu rectangle*)
  Constants.menu_rect :=
    Some
      (Rectangle.create
         (29.5 *. floor (!screen_width /. 40.))
         (!screen_height /. 35.)
         (7. *. floor (!screen_width /. 28.))
         (38. *. !screen_height /. 40.));
  (*Make the selection info GUI rectangle*)
  Constants.selection_rect :=
    Some
      (Rectangle.create
         (29.5 *. floor (!screen_width /. 40.))
         (!screen_height /. 2.6)
         (7. *. floor (!screen_width /. 28.))
         (15. *. !screen_height /. 40.));

  (* Setup lives and cash images *)
  Constants.heart_img :=
    Some Raylib.(load_texture_from_image (load_image "./img/heart.png"));

  Constants.cash_img :=
    Some Raylib.(load_texture_from_image (load_image "./img/dollar.png"));

  Constants.pop_img :=
    Some (Raylib.load_texture_from_image (Raylib.load_image "./img/pop.png"));

  path_rectangles := rect_json_parse ();

  (*Turn points on the path*)
  turn_points := point_json_parse ();

  (*Load all the waves for the game.*)
  waves :=
    [
      Waves.wave1;
      Waves.wave2;
      Waves.wave3;
      Waves.wave4;
      Waves.wave5;
      Waves.wave6;
      Waves.wave7;
      Waves.wave8;
      Waves.wave9;
      Waves.wave10;
      Waves.wave11;
      Waves.wave12;
      Waves.wave13;
      Waves.wave14;
      Waves.wave15;
      Waves.wave16;
      Waves.wave17;
      Waves.wave18;
      Waves.wave19;
      Waves.wave20;
      Waves.wave21;
      Waves.wave22;
      Waves.wave23;
      Waves.wave24;
      Waves.wave25;
    ]

(******************************************************************************)

(**Adds bloons that are ready to be added to the screen, to current_balloons. If
   none are ready, decreases the counter on the next balloon to be added.*)
let bloons_spawner current_wave =
  match !current_wave with
  | [] -> ()
  | (bloon, counter) :: t when counter <= 0 ->
      current_balloons := bloon :: !current_balloons;
      current_wave := t
  | (bloon, counter) :: t ->
      current_wave := (bloon, counter - (1 * !speed_mult)) :: t

(* If there are no balloons on the screen, the round is over. *)
let update_state () =
  if !current_balloons = [] && !current_wave = [] && !Constants.state = Active
  then (
    Projectiles.bullet_collection := [];
    Constants.state := Inactive;
    Constants.cash := !Constants.cash + 100;
    Constants.round := !Constants.round + 1)

(******************************************************************************)
let update_game () =
  update_state ();
  Menubar.check_click ();
  Menubar.check_hover ();
  Menubar.place_bear ();
  bear_collection := Bears.remove_bears !bear_collection;

  Bears.update_selected_bear !selected_bear (get_mouse_position ());

  if !Constants.state = Active then (
    bloons_spawner current_wave;
    move_balloons !current_balloons !turn_points;
    fire_all_shots !bear_collection (sort_balloons !current_balloons);
    update_bullets !bullet_collection;

    (*Update all bear facing angles!*)
    update_angles !bear_collection (sort_balloons !current_balloons);

    (* update_bears_angle !bear_collection !current_balloons; *)
    update_collisions !bullet_collection !current_balloons;
    bullet_collection := Projectiles.remove_bullets !bullet_collection;
    current_balloons := Balloons.remove_balloons !current_balloons;

    if !lives <= 0 then Constants.state := Lose)
  else if !Constants.state = Lose then Raylib.clear_background Color.white

(******************************************************************************)
let draw_game () =
  let open Raylib in
  begin_drawing ();
  clear_background Color.white;
  (*Draw the background & reference grid*)
  Gamebackground.draw_background (Option.get !game_background_img);

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

  Menubar.draw_heart !screen_width !screen_height;
  Menubar.draw_cash !screen_width !screen_height;
  Menubar.lives_and_cash_count !screen_width !screen_height;

  (* Drawing round button *)
  if !Constants.state <> Active then play_button !screen_width !screen_height;

  mult_button !screen_width !screen_height;

  (*Draw the SELECTED bear to PLACE*)
  Bears.draw_selected_bear !selected_bear;

  (*Draws the range of the selected bear: grey if it is in a valid location,
     red otherwise.*)
  if !selected then Menubar.draw_range !selected_bear;
  Menubar.draw_hover_highlight ();
  (*Draw PLACED bears!*)
  Bears.draw_bears !Bears.bear_collection;

  (*Draw bullets*)
  draw_bullets !bullet_collection;

  (*Draw the turning points for reference, comment out if you want them invisible*)
  (* Balloonpath.draw_turnpoints !turn_points; *)

  (*Draw the balloons, the number passed in is the path's width, so that balloons
     are drawn as the correct size.*)
  if !Constants.state = Active then
    Balloons.draw_balloons (2. *. !screen_height /. 28.) !current_balloons;

  Menubar.draw_hover_highlight ();

  (*Draw the information panel based on what was last clicked and/or hovered over.*)
  Menubar.display_bear_info !Menubar.select_display !Menubar.hover_display;
  if !showInstructions then (
    draw_rectangle 0 0
      (int_of_float !screen_width)
      (int_of_float !screen_height)
      (Color.create 0 0 0 200);
    if
      let x_pos = 1. *. !screen_width /. 9. in
      let y_pos = 1. *. !screen_height /. 9. in
      let show_window =
        Raygui.window_box
          (Rectangle.create x_pos y_pos
             (4. *. !screen_width /. 5.)
             (4. *. !screen_height /. 5.))
          ""
      in
      draw_text_ex (Option.get !game_font)
        "\t\t\t\t\t\t\t\t\t\t\t\t\t\tWelcome to McGraw Tower Defense!\n\n\
         \t\t\t\t\t\tDefend Cornell and McGraw Tower from waves of \n\
         \t\t\t\t\t\t\toncoming balloons. You earn cash for every \n\
         \t\t\t\t\t\t\tlayer of balloon that you pop and at the end \n\
         \t\t\t\t\t\t\tof each round. Use it strategically to buy and \n\
         \t\t\t\t\t\t\tupgrade bears. \n\n\
        \         \t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tGood luck!"
        (Vector2.create (x_pos +. 10.) (y_pos +. 30.))
        60. 2. Color.white;
      show_window
    then showInstructions := false)
  else if (*RESTART GAME********************************)
          !Constants.state = Lose
  then (
    draw_rectangle 0 0
      (int_of_float !screen_width)
      (int_of_float !screen_height)
      (Color.create 150 0 0 180);
    if
      let x_pos = 1. *. !screen_width /. 9. in
      let y_pos = 1. *. !screen_height /. 9. in
      let show_window =
        Raygui.window_box
          (Rectangle.create x_pos y_pos
             (4. *. !screen_width /. 5.)
             (4. *. !screen_height /. 5.))
          ""
      in
      draw_text_ex (Option.get !game_font)
        "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tYou Lost!\n\n\n\
        \        \t\t\t\t\t\t\t\t\t\t\t\t\t\tclose this window to try again!"
        (Vector2.create (x_pos +. 10.) (y_pos +. 30.))
        45. 2. Color.white;
      show_window
    then (
      Constants.state := Home;
      count := 0));
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
