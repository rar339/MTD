open Raylib
open Raygui
open Constants

let count = ref 0
let showInstructions = ref true

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

module ValidRectangles = struct
  let rect_color = Color.create 0 0 0 100
  let path_rectangles : Rectangle.t list ref = ref []

  (* let rect_one =
     Rectangle.create 0.
       (3. *. float_of_int (get_screen_height ()) /. 28.)
       (float_of_int (get_screen_width ()))
       (3. *. float_of_int (get_screen_height ()) /. 28.) *)

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
module BalloonPath = struct
  (*Points are represetned as pairs of ints.*)
  let start_point = (0, 0)

  (*If a baloon is ever at a negative y value, it has reached the end of the path.*)
  let end_line = -10

  (*start_point should be changed to be somewhere off the screen*)
  let turn_points : (int * int) list ref = ref []
  let draw_turnpoint x_pos y_pos = draw_circle x_pos y_pos 10.0 Color.red

  let rec draw_turnpoints (turn_points : (int * int) list) =
    match turn_points with
    | [] -> ()
    | (x, y) :: t ->
        draw_turnpoint x y;
        draw_turnpoints t
end

open GameBackground
open ValidRectangles
open BalloonPath

let setup () =
  (*Setup backgrounds*)
  Raygui.set_style (Default `Text_size) 30;
  Raygui.set_style (Default `Background_color) 0x6699CC;
  Raygui.(set_style (Label `Text_color_normal) 0xFFFFFFFF);

  let game_image : Image.t = Raylib.load_image "mtd_map.png" in
  background := Some (load_texture_from_image game_image);
  background_width := Image.width game_image;
  background_height := Image.height game_image;
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

  (*Turn points on the path*)
  turn_points :=
    [
      ( 22 * round_float (!screen_width /. 40.),
        3 * round_float (!screen_height /. 28.) );
      ( 22 * round_float (!screen_width /. 40.),
        8 * round_float (!screen_height /. 28.) );
      ( 4 * round_float (!screen_width /. 40.),
        8 * round_float (!screen_height /. 28.) );
      ( 4 * round_float (!screen_width /. 40.),
        26 * round_float (!screen_height /. 28.) );
      ( 25 * round_float (!screen_width /. 40.),
        26 * round_float (!screen_height /. 28.) );
      ( 25 * round_float (!screen_width /. 40.),
        21 * round_float (!screen_height /. 28.) );
      ( 9 * round_float (!screen_width /. 40.),
        21 * round_float (!screen_height /. 28.) );
      ( 9 * round_float (!screen_width /. 40.),
        13 * round_float (!screen_height /. 28.) );
      ( 14 * round_float (!screen_width /. 40.),
        13 * round_float (!screen_height /. 28.) );
      ( 14 * round_float (!screen_width /. 40.),
        16 * round_float (!screen_height /. 28.) );
      ( 27 * round_float (!screen_width /. 40.),
        16 * round_float (!screen_height /. 28.) );
    ]

let update_game () =
  Raygui.set_style (Label `Base_color_normal) 100;
  ()

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
  ValidRectangles.draw_rectangles !path_rectangles;

  Raygui.set_style (Default `Background_color) 0x99CCFF;
  Raygui.set_style (Label `Base_color_normal) 100;
  (* Raygui.set_style (Label `)  100; *)
  Raygui.set_style (Label `Text_color_normal) 100;

  (*Draw the turning points for reference, comment out if you want them invisible*)
  BalloonPath.draw_turnpoints !turn_points;

  if !showInstructions then
    if
      let x_pos = (!screen_width /. 2.) -. 300. in
      let y_pos = (!screen_height /. 2.) -. 300. in
      let show_window =
        window_box
          (Rectangle.create (*Magic number to offset window location: 300*)
             x_pos y_pos 800. 600.)
          ""
      in
      draw_text "Hello, welcome to McGraw Tower Defense..."
        (int_of_float (x_pos +. 10.))
        (int_of_float (y_pos +. 30.))
        30 Color.red;
      show_window
    then showInstructions := false;

  end_drawing ()

(*Main game loop*)
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
