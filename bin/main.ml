let () = print_endline "Starting MTD!"

let setup () =
  Raylib.init_window 700 450 "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    let loading_screen = Raylib.load_image "loadscreen.png" in
    let texture = Raylib.load_texture_from_image loading_screen in
    ignore (texture);
    begin_drawing ();
    clear_background Color.raywhite;
    (draw_texture_ex texture       
    (Vector2.create 0. 0.0)  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.42)                (* Scale *)
    Color.white);
    draw_text "McGraw Tower" 40 200 50
      Color.red;
    draw_text "Defense" 40 250 50 Color.red;
    end_drawing ();
    loop ()

let () = setup () |> loop