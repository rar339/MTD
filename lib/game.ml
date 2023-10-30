let width = ref 0
let height = ref 0
let count = ref 0

open Raylib
open Constants

let background : Texture2D.t option ref = ref None

let setup () =
  let game_image : Image.t ref = ref (Raylib.load_image "mtd_map.png") in
  background := Some (load_texture_from_image !game_image);

  ()

let update_game () = ()

let draw_game () =
  let open Raylib in
  begin_drawing ();
  clear_background Color.red;

  draw_texture_ex (Option.get !background)
    (Vector2.create 0. 0.0) (* Position *)
    0.0 (* Rotation (in radians) *)
    (float_of_int !screen_width /. 2388.0) (* Scale *)
    Color.white;
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
