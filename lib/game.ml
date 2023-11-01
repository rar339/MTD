open Raylib
open Raygui
open Constants

let count = ref 0
let showInstructions = ref true
let selected : bool ref = ref false

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
      (Color.create 255 255 255 255);
    ()

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
  let menu_rect : Rectangle.t option ref = ref None

  let draw_menu rect =
    draw_rectangle_rec rect (Color.create 183 201 226 255);
    draw_rectangle_lines_ex rect 3. Color.black;
    ()

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
    then print_endline "START"
end
(******************************************************************************)

(******************************************************************************)
module BalloonPath = struct
  (*Points are represetned as triplet of ints, where the third int is the number
     corresponding to that turn.*)
  let start_point = (0, 0, 0)

  (*If a baloon is ever at a negative y value, it has reached the end of the path.*)
  let end_line = -10

  (*start_point should be changed to be somewhere off the screen*)
  let turn_points : (int * int * int) list ref = ref []
  let draw_turnpoint x_pos y_pos = draw_circle x_pos y_pos 10.0 Color.red

  let rec draw_turnpoints (turn_points : (int * int * int) list) =
    match turn_points with
    | [] -> ()
    | (x, y, _) :: t ->
        draw_turnpoint x y;
        draw_turnpoints t

  let turn_balloon (x, y, id) =
    match (x, y, id) with
    (* | x, y, 1 -> check_collision_circle_rec  *)
    (* | x, y, 2 -> ()
       | x, y, 3 -> ()
       | x, y, 4 -> ()
       | x, y, 5 -> ()
       | x, y, 6 -> ()
       | x, y, 7 -> ()
       | x, y, 8 -> ()
       | x, y, 9 -> ()
       | x, y, 10 -> () *)
    | _ -> failwith "impossible"
end

(******************************************************************************)
open GameBackground
open GameBounds
open MenuBar
open BalloonPath

let setup () =
  (*Setup backgrounds and Raygui*)
  Raygui.set_style (Default `Text_size) 30;
  (* Raygui.set_style (Default `Base_color_normal) 0x6699CC; *)
  Raygui.set_style (Default `Background_color) 0x6699CC;
  (* Raygui.set_style (Default `Border_color_normal) 0x00000; *)
  (* Raygui.set_style (Default `Border_width) 2; *)
  Raygui.(set_style (Label `Text_color_normal) 0xFFFFFFFF);

  (*Setup background image*)
  let game_image : Image.t = Raylib.load_image "mtd_map.png" in
  background := Some (load_texture_from_image game_image);
  background_width := Image.width game_image;
  background_height := Image.height game_image;

  (*Make the menu rectangle*)
  menu_rect :=
    Some
      (Rectangle.create
         (29.5 *. floor (!screen_width /. 40.))
         (!screen_height /. 35.)
         (7. *. floor (!screen_width /. 28.))
         (38. *. !screen_height /. 40.));

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
      ( 22 * round_float (!screen_width /. 40.),
        3 * round_float (!screen_height /. 28.),
        1 );
      ( 22 * round_float (!screen_width /. 40.),
        8 * round_float (!screen_height /. 28.),
        2 );
      ( 4 * round_float (!screen_width /. 40.),
        8 * round_float (!screen_height /. 28.),
        3 );
      ( 4 * round_float (!screen_width /. 40.),
        26 * round_float (!screen_height /. 28.),
        4 );
      ( 25 * round_float (!screen_width /. 40.),
        26 * round_float (!screen_height /. 28.),
        5 );
      ( 25 * round_float (!screen_width /. 40.),
        21 * round_float (!screen_height /. 28.),
        6 );
      ( 9 * round_float (!screen_width /. 40.),
        21 * round_float (!screen_height /. 28.),
        7 );
      ( 9 * round_float (!screen_width /. 40.),
        13 * round_float (!screen_height /. 28.),
        8 );
      ( 14 * round_float (!screen_width /. 40.),
        13 * round_float (!screen_height /. 28.),
        9 );
      ( 14 * round_float (!screen_width /. 40.),
        16 * round_float (!screen_height /. 28.),
        10 );
      ( 27 * round_float (!screen_width /. 40.),
        16 * round_float (!screen_height /. 28.),
        11 );
    ]

(******************************************************************************)
let update_game () = ()

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
  MenuBar.play_button !screen_width !screen_height;

  (*Draw the BEAR reference images*)
  Bears.draw_dart_bear_img !screen_width !screen_height;

  
  (*Draw the turning points for reference, comment out if you want them invisible*)
  BalloonPath.draw_turnpoints !turn_points;

  if !showInstructions then (
    draw_rectangle 0 0
      (int_of_float !screen_width)
      (int_of_float !screen_height)
      (Color.create 0 0 0 200);
    if
      let x_pos = 1. *. !screen_width /. 5. in
      let y_pos = 1. *. !screen_height /. 5. in
      let show_window =
        window_box
          (Rectangle.create (*Magic number to offset window location: 300*)
             x_pos y_pos
             (3. *. !screen_width /. 5.)
             (3. *. !screen_height /. 5.))
          ""
      in

      draw_text "Hello, welcome to McGraw Tower Defense..."
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
