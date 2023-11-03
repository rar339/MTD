open Raylib
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