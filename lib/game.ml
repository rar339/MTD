open Raylib
open Constants

let count = ref 0
let background_width : int ref = ref 0
let background_height : int ref = ref 0
let background : Texture2D.t option ref = ref None

let setup () =
  let game_image : Image.t = Raylib.load_image "mtd_map.png" in
  background := Some (load_texture_from_image game_image);
  background_width := Image.width game_image;
  background_height := Image.height game_image;
  ()

let update_game () = ()

let draw_game () =
  let open Raylib in
  begin_drawing ();
  clear_background Color.red;

  draw_texture_pro (Option.get !background)
    (Rectangle.create 0. 0. 2388. 1668.)
    (Rectangle.create 0. 0.
       (float_of_int !screen_width)
       (float_of_int !screen_height))
    (Vector2.create 0. 0.) 0.
    (Color.create 255 255 255 255);

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
