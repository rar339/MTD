open Raylib

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
