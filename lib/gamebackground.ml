open Raylib
open Constants

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
