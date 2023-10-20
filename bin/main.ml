let () = print_endline ""
let () = print_endline "*************************************"
let () = print_endline "********** Starting MTD! ************"
let () = print_endline "*************************************"
let () = print_endline ""

open Raylib
let setup () =
  Raylib.init_window 900 650 "MTD";
  Raylib.set_target_fps 60;


  (*Create the intro screen art*)
  let intro_screen_art = Raylib.load_image "MTDCoverArt.png" in 
  let texture = Raylib.load_texture_from_image intro_screen_art in
  unload_image intro_screen_art;

  let red_balloon = Raylib.load_image "red.png" in
  let red_bal_texture = Raylib.load_texture_from_image red_balloon in 
  unload_image red_balloon;

  (texture, red_bal_texture)

let pos = ref 550.

let update_balloon_position(pos) = 
  if !pos < 2. then pos:= 650. else pos:= !pos -. 0.09; pos 

let rec loop (tuple) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
   
    begin_drawing ();
    clear_background Color.raywhite;

    match tuple with 
    | (a, b) -> let texture = a in
    let red_bal_texture = b in

    (***** BACKGROUND *****)
    (draw_texture_ex texture       
    (Vector2.create 0. 0.0)  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.70)                (* Scale *)
    Color.white);

    draw_text "McGraw Tower" 430 175 60
      Color.red;
    draw_text "Defense" 490 250 60 Color.red;

    Raygui.(set_style (TextBox `Text_alignment) TextAlignment.(to_int Center));
    (* SETTING STYLE TO RED - USE HEX*)
    Raygui.(set_style (Button `Base_color_normal) 0xFF000010);
    Raygui.(set_style (Button `Text_color_normal) 0);
    Raygui.(set_style (Button `Text_padding) 0);

    (* create -> x y width height*)
    Raygui.(ignore (button (Rectangle.create 550. 360. 120. 50.) "PLAY"));


    (***** BALLOONS *****)
    (draw_texture_ex red_bal_texture       
    (Vector2.create (500.) !(update_balloon_position(pos)))  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.15)                (* Scale *)
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (600.) (50. +. !(update_balloon_position(pos))))  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.15)                (* Scale *)
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (300.) (300. +. !(update_balloon_position(pos))))  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.15)                (* Scale *)
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (700.) (100. +. !(update_balloon_position(pos))))  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.15)                (* Scale *)
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (740.) (200. +. !(update_balloon_position(pos))))  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.15)                (* Scale *)
    Color.white);

    end_drawing ();
    loop (tuple)

let () = setup () |> loop