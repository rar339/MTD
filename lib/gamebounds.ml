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

let generate_rectangles screen_width screen_height =
  [
    create_rectangle 0.
      (2. *. floor (!screen_height /. 28.))
      (23. *. floor (!screen_width /. 40.))
      (2. *. floor (!screen_height /. 28.));
    create_rectangle
      (21. *. floor (!screen_width /. 40.))
      (4. *. floor (!screen_height /. 28.))
      (2. *. floor (!screen_width /. 40.))
      (5. *. floor (!screen_height /. 28.));
    create_rectangle
      (3. *. floor (!screen_width /. 40.))
      (7. *. floor (!screen_height /. 28.))
      (18. *. floor (!screen_width /. 40.))
      (2. *. floor (!screen_height /. 28.));
    create_rectangle
      (3. *. floor (!screen_width /. 40.))
      (9. *. floor (!screen_height /. 28.))
      (2. *. floor (!screen_width /. 40.))
      (19. *. floor (!screen_height /. 29.));
    create_rectangle
      (5. *. floor (!screen_width /. 40.))
      (26. *. floor (!screen_height /. 29.))
      (19. *. floor (!screen_width /. 40.))
      (3. *. floor (!screen_height /. 38.));
    create_rectangle
      (24. *. floor (!screen_width /. 40.))
      (18. *. floor (!screen_height /. 25.))
      (2. *. floor (!screen_width /. 40.))
      (8. *. floor (!screen_height /. 31.));
    create_rectangle
      (8. *. floor (!screen_width /. 40.))
      (12. *. floor (!screen_height /. 28.))
      (2. *. floor (!screen_width /. 40.))
      (11. *. floor (!screen_height /. 30.));
    create_rectangle
      (10. *. floor (!screen_width /. 40.))
      (24. *. floor (!screen_height /. 33.))
      (14. *. floor (!screen_width /. 40.))
      (3. *. floor (!screen_height /. 37.));
    create_rectangle
      (10. *. floor (!screen_width /. 40.))
      (12. *. floor (!screen_height /. 28.))
      (3. *. floor (!screen_width /. 40.))
      (3. *. floor (!screen_height /. 36.));
    create_rectangle
      (13. *. floor (!screen_width /. 40.))
      (12. *. floor (!screen_height /. 28.))
      (2. *. floor (!screen_width /. 40.))
      (5. *. floor (!screen_height /. 27.));
    create_rectangle
      (15. *. floor (!screen_width /. 40.))
      (15. *. floor (!screen_height /. 28.))
      (13. *. floor (!screen_width /. 40.))
      (2. *. floor (!screen_height /. 25.));
    create_rectangle
      (26. *. floor (!screen_width /. 40.))
      (0. *. floor (!screen_height /. 28.))
      (2. *. floor (!screen_width /. 36.))
      (15. *. floor (!screen_height /. 28.));
    (*This is the menubar rectangle.*)
    create_rectangle
      (22. *. floor (!screen_width /. 30.))
      0.
      (8. *. floor (!screen_width /. 28.))
      !screen_height;
  ]
