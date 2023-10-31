open Raylib
open Constants

let count = ref 0

(******************************************************************************)
module GameBackground = struct
  let background : Texture2D.t option ref = ref None
  let background_width : int ref = ref 0
  let background_height : int ref = ref 0

  let draw_background (background_art : Texture2D.t option ref) =
    draw_texture_pro
      (Option.get !background_art)
      (Rectangle.create 0. 0. 2388. 1668.)
      (Rectangle.create 0. 0.
         (float_of_int !screen_width)
         (float_of_int !screen_height))
      (Vector2.create 0. 0.) 0.
      (Color.create 255 255 255 255);
    ()

  let draw_ref_grid width height =
    let x = ref 0 in
    let y = ref 0 in
    while !x < width do
      draw_line !x 0 !x height Color.black;
      x := !x + (width / 42);
      draw_line 0 !y width !y Color.black;
      y := !y + (height / 28)
    done
end

(******************************************************************************)
open GameBackground

let setup () =

  (*Setup backgrounds*)
  let game_image : Image.t = Raylib.load_image "mtd_map.png" in
  background := Some (load_texture_from_image game_image);
  background_width := Image.width game_image;
  background_height := Image.height game_image;
  ()

let update_game () = ()

let draw_game () =
  let open Raylib in
  begin_drawing ();
  clear_background Color.white;

  (*Draw the background & reference grid*)
  GameBackground.draw_background background;
  GameBackground.draw_ref_grid !screen_width !screen_height;

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
