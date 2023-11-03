open Raylib
open Constants

let count = ref 0
let showInstructions = ref true
let selected : bool ref = ref false
let selected_bear : Bears.bear option ref = ref None

(*A list of all the waves in our game.*)
let waves = ref []

(*The current wave of baloons, when this is the empty list, the wave is over*)
let current_wave = ref []

let initialize_round waves () =
  match !waves with
  | [] -> ()
  | h :: t ->
      current_wave := h;
      waves := t

(*The current list of balloons that are actually on the screen.*)
let current_bloons = ref []

(******************************************************************************)
module GameBackground = struct
  let background : Texture2D.t option ref = ref None
  let background_width : int ref = ref 0
  let background_height : int ref = ref 0

  let draw_background (background_art : Texture2D.t option ref) =
    draw_texture_pro
      (Option.get !background_art)
      (Rectangle.create 0. 0. 2388. 1668.)
      (Rectangle.create 0. 0. !screen_width !screen_height)
      (Vector2.create 0. 0.) 0.
      (Color.create 255 255 255 255)

  let draw_ref_grid width height =
    let x = ref 0 in
    let y = ref 0 in
    while !x < width do
      draw_line !x 0 !x height Color.black;
      x := !x + (width / 40);
      draw_line 0 !y width !y Color.black;
      y := !y + (height / 28)
    done
end

(******************************************************************************)
module GameBounds = struct
  let rect_color = Color.create 0 0 0 100
  let path_rectangles : Rectangle.t list ref = ref []

  let create_rectangle x_pos y_pos width height =
    Rectangle.create x_pos y_pos width height

  let rec draw_rectangles (rectangles : Rectangle.t list) =
    match rectangles with
    | [] -> ()
    | h :: t ->
        Raylib.draw_rectangle_rec h rect_color;
        draw_rectangles t
end

(******************************************************************************)
module MenuBar = struct
  let menu_rect = ref None
  let heart_img = ref None
  let cash_img = ref None

  let draw_menu rect =
    draw_rectangle_rec rect (Color.create 183 201 226 255);
    draw_rectangle_lines_ex rect 3. Color.black;
    ()

  let lives_box screen_width screen_height =
    Rectangle.create
      (149. *. screen_width /. 200.)
      (0.5 *. screen_height /. 9.)
      (1. *. screen_width /. 9.)
      (screen_height /. 19.)

  let cash screen_width screen_height =
    Rectangle.create
      (173. *. screen_width /. 200.)
      (0.5 *. screen_height /. 9.)
      (1. *. screen_width /. 9.)
      (screen_height /. 19.)

  let draw_heart heart_text screen_width screen_height =
    draw_texture_ex (Option.get heart_text)
      (Vector2.create
         (149. *. screen_width /. 200.)
         (0.45 *. screen_height /. 9.))
      0. 0.10 Color.white

  let draw_cash cash_text screen_width screen_height =
    draw_texture_ex (Option.get cash_text)
      (Vector2.create
         (174. *. screen_width /. 200.)
         (0.55 *. screen_height /. 9.))
      0. 0.07 Color.white

  let lives_and_cash_count screen_width screen_height =
    Raylib.draw_text
      (string_of_int !Constants.lives)
      (int_of_float (158. *. screen_width /. 200.))
      (int_of_float (0.62 *. screen_height /. 9.))
      25 Color.white;
    Raylib.draw_text
      (string_of_int !Constants.cash)
      (int_of_float (182. *. screen_width /. 200.))
      (int_of_float (0.62 *. screen_height /. 9.))
      25 Color.white

  let play_button screen_width screen_height =
    if
      Raygui.(
        button
          (Rectangle.create
             (149. *. screen_width /. 200.)
             (8. *. screen_height /. 9.)
             (2. *. screen_width /. 9.)
             (screen_height /. 19.))
          "Start Round")
    then (
      initialize_round waves ();
      Constants.state := Active)
end

(******************************************************************************)
module BearCollection = struct
  let bear_collection : Bears.bear list ref = ref []
end
(******************************************************************************)

module BalloonPath = struct
  (*Points are represetned as pairs of ints.*)
  let start_point = (0, 0)

  (*start_point should be changed to be somewhere off the screen*)
  let turn_points : (int * int * int) list ref = ref []
  let draw_turnpoint x_pos y_pos = draw_circle x_pos y_pos 1.0 Color.red

  let rec draw_turnpoints (turn_points : (int * int * int) list) =
    match turn_points with
    | [] -> ()
    | (x, y, _) :: t ->
        draw_turnpoint x y;
        draw_turnpoints t

  (*Checks if the given balloon is colliding with a turn point, meaning it should
     make a turn. *)
  let rec check_turn_collide (balloon : Balloons.balloon)
      (turn_pts : (int * int * int) list) =
    match turn_pts with
    | [] -> None
    | (x, y, i) :: t ->
        if
          check_collision_circle_rec
            (Vector2.create (float_of_int x) (float_of_int y))
            1.
            (Balloons.get_hitbox (2. *. !screen_height /. 28.) balloon)
          && balloon.current_turn < i
        then (
          balloon.current_turn <- balloon.current_turn + 1;
          Some i)
        else check_turn_collide balloon t

  let turn_balloon rate i =
    match i with
    | 1 -> Vector2.create 0.0 rate
    | 2 -> Vector2.create (-.rate) 0.0
    | 3 -> Vector2.create 0.0 (-.rate)
    | 4 -> Vector2.create rate 0.0
    | 5 -> Vector2.create 0.0 (-.rate)
    | 6 -> Vector2.create rate 0.0
    | 7 -> Vector2.create 0.0 rate
    | 8 -> Vector2.create (-.rate) 0.0
    | 9 -> Vector2.create 0.0 rate
    | 10 -> Vector2.create rate 0.0
    | 11 -> Vector2.create 0.0 (-.rate)
    | _ -> failwith "impossible"

  (*Moves the balloon, taking into consideration if a turn is reached. If a turn
     is reached, changes the velocity but does not update position.*)
  let move_balloon (balloon : Balloons.balloon) turn_pts =
    let x = Vector2.x balloon.position in
    let y = Vector2.y balloon.position in
    let x_rate = Vector2.x balloon.velocity in
    let y_rate = Vector2.y balloon.velocity in
    match check_turn_collide balloon turn_pts with
    | None -> balloon.position <- Vector2.create (x +. x_rate) (y +. y_rate)
    | Some i ->
        balloon.velocity <-
          turn_balloon (if x_rate = 0.0 then y_rate else x_rate) i

  let rec move_balloons (balloon_list : Balloons.balloon list) turn_pts =
    match balloon_list with
    | [] -> ()
    | h :: t ->
        move_balloon h turn_pts;
        move_balloons t turn_pts
end

(******************************************************************************)
open GameBackground
open GameBounds
open MenuBar
open BalloonPath
open BearCollection

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
  background := Some (load_texture_from_image game_image);
  background_width := Image.width game_image;
  background_height := Image.height game_image;

  (* Setup heart and cash images *)

  (*Make the menu rectangle*)
  menu_rect :=
    Some
      (Rectangle.create
         (29.5 *. floor (!screen_width /. 40.))
         (!screen_height /. 35.)
         (7. *. floor (!screen_width /. 28.))
         (38. *. !screen_height /. 40.));

  heart_img :=
    Some Raylib.(load_texture_from_image (load_image "./img/heart.png"));

  cash_img :=
    Some Raylib.(load_texture_from_image (load_image "./img/dollar.png"));

  path_rectangles :=
    create_rectangle 0.
      (2. *. floor (!screen_height /. 28.))
      (23. *. floor (!screen_width /. 40.))
      (2. *. floor (!screen_height /. 28.))
    :: create_rectangle
         (21. *. floor (!screen_width /. 40.))
         (4. *. floor (!screen_height /. 28.))
         (2. *. floor (!screen_width /. 40.))
         (5. *. floor (!screen_height /. 28.))
    :: create_rectangle
         (3. *. floor (!screen_width /. 40.))
         (7. *. floor (!screen_height /. 28.))
         (18. *. floor (!screen_width /. 40.))
         (2. *. floor (!screen_height /. 28.))
    :: create_rectangle
         (3. *. floor (!screen_width /. 40.))
         (9. *. floor (!screen_height /. 28.))
         (2. *. floor (!screen_width /. 40.))
         (19. *. floor (!screen_height /. 29.))
    :: create_rectangle
         (5. *. floor (!screen_width /. 40.))
         (26. *. floor (!screen_height /. 29.))
         (19. *. floor (!screen_width /. 40.))
         (3. *. floor (!screen_height /. 38.))
    :: create_rectangle
         (24. *. floor (!screen_width /. 40.))
         (18. *. floor (!screen_height /. 25.))
         (2. *. floor (!screen_width /. 40.))
         (8. *. floor (!screen_height /. 31.))
    :: create_rectangle
         (8. *. floor (!screen_width /. 40.))
         (12. *. floor (!screen_height /. 28.))
         (2. *. floor (!screen_width /. 40.))
         (11. *. floor (!screen_height /. 30.))
    :: create_rectangle
         (10. *. floor (!screen_width /. 40.))
         (24. *. floor (!screen_height /. 33.))
         (14. *. floor (!screen_width /. 40.))
         (3. *. floor (!screen_height /. 37.))
    :: create_rectangle
         (10. *. floor (!screen_width /. 40.))
         (12. *. floor (!screen_height /. 28.))
         (3. *. floor (!screen_width /. 40.))
         (3. *. floor (!screen_height /. 36.))
    :: create_rectangle
         (13. *. floor (!screen_width /. 40.))
         (12. *. floor (!screen_height /. 28.))
         (2. *. floor (!screen_width /. 40.))
         (5. *. floor (!screen_height /. 27.))
    :: create_rectangle
         (15. *. floor (!screen_width /. 40.))
         (15. *. floor (!screen_height /. 28.))
         (13. *. floor (!screen_width /. 40.))
         (2. *. floor (!screen_height /. 25.))
    :: create_rectangle
         (26. *. floor (!screen_width /. 40.))
         (0. *. floor (!screen_height /. 28.))
         (2. *. floor (!screen_width /. 36.))
         (15. *. floor (!screen_height /. 28.))
    :: !path_rectangles;

  (*Create the ref rectangle for where the menu is
    The side bar should be an invalid place for bears*)
  path_rectangles :=
    create_rectangle
      (22. *. floor (!screen_width /. 30.))
      0.
      (8. *. floor (!screen_width /. 28.))
      !screen_height
    :: !path_rectangles;

  (*Turn points on the path*)
  turn_points :=
    [
      ( 22 * round_float (!screen_width /. 39.),
        3 * round_float (!screen_height /. 28.),
        1 );
      ( 22 * round_float (!screen_width /. 40.),
        8 * round_float (!screen_height /. 27.),
        2 );
      ( 4 * round_float (!screen_width /. 48.),
        8 * round_float (!screen_height /. 28.),
        3 );
      ( 4 * round_float (!screen_width /. 48.),
        26 * round_float (!screen_height /. 27.),
        4 );
      ( 25 * round_float (!screen_width /. 40.),
        26 * round_float (!screen_height /. 28.),
        5 );
      ( 25 * round_float (!screen_width /. 40.),
        21 * round_float (!screen_height /. 28.5),
        6 );
      ( 9 * round_float (!screen_width /. 42.),
        21 * round_float (!screen_height /. 28.5),
        7 );
      ( 9 * round_float (!screen_width /. 42.),
        13 * round_float (!screen_height /. 29.),
        8 );
      ( 14 * round_float (!screen_width /. 40.),
        13 * round_float (!screen_height /. 29.),
        9 );
      ( 14 * round_float (!screen_width /. 40.),
        16 * round_float (!screen_height /. 28.),
        10 );
      ( 27 * round_float (!screen_width /. 39.),
        16 * round_float (!screen_height /. 28.),
        11 );
    ];

  (*Load all the waves for the game.*)
  waves := [ Waves.wave1 screen_height; Waves.wave2 screen_height ]
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
  if !current_bloons = [] && !current_wave = [] then Constants.state := Inactive

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
let place_bear () =
  if
    !selected = false
    && is_mouse_button_pressed Left
    && Bears.determine_ref_bear_clicked (get_mouse_position ()) !screen_width
         !screen_height
  then (
    selected_bear := Some (Bears.make_dart_bear (get_mouse_position ()));
    selected := true)
  else if
    nevermind (get_mouse_position ()) (Option.get !menu_rect)
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
  then (
    selected := false;
    bear_collection := Option.get !selected_bear :: !bear_collection;
    Constants.cash := !Constants.cash - (Option.get !selected_bear).cost;
    selected_bear := None)

let update_game () =
  update_state ();
  place_bear ();

  Bears.update_selected_bear !selected_bear (get_mouse_position ());

  if !Constants.state = Active then (
    bloons_spawner current_wave;
    move_balloons !current_bloons !turn_points;
    current_bloons := Balloons.remove_out_of_bounds !current_bloons)

(******************************************************************************)
let draw_game () =
  let open Raylib in
  begin_drawing ();
  clear_background Color.white;

  (*Draw the background & reference grid*)
  GameBackground.draw_background background;

  GameBackground.draw_ref_grid
    (int_of_float !screen_width)
    (int_of_float !screen_height);

  (*This line shows ref rectangles! Comment out if you want them invisible*)
  GameBounds.draw_rectangles !path_rectangles;

  MenuBar.draw_menu (Option.get !menu_rect);

  (*** Drawing lives and cash ***)
  Raylib.draw_rectangle_rec
    (MenuBar.lives_box !screen_width !screen_height)
    (Color.create 150 0 0 100);

  Raylib.draw_rectangle_rec
    (MenuBar.cash !screen_width !screen_height)
    (Color.create 0 150 0 100);

  MenuBar.draw_heart !heart_img !screen_width !screen_height;
  MenuBar.draw_cash !cash_img !screen_width !screen_height;
  MenuBar.lives_and_cash_count !screen_width !screen_height;

  (* Drawing round button *)
  if !Constants.state <> Active then
    MenuBar.play_button !screen_width !screen_height;

  (*Draw the SELECTED bear to PLACE*)
  Bears.draw_selected_bear !selected_bear;

  (*Draw the MENU bears*)
  Bears.draw_dart_bear_img
    (6. *. !screen_width /. 7.)
    (1. *. !screen_height /. 4.);

  if !selected then
    draw_circle
      (Constants.round_float (Vector2.x (Option.get !selected_bear).position))
      (Constants.round_float (Vector2.y (Option.get !selected_bear).position))
      (Option.get !selected_bear).range (Color.create 0 0 0 100);

  (*Draw PLACED bears!*)
  Bears.draw_bears !bear_collection;

  (*Draw the turning points for reference, comment out if you want them invisible*)
  BalloonPath.draw_turnpoints !turn_points;

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
          (Rectangle.create (*Magic number to offset window location: 300*)
             x_pos y_pos
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
