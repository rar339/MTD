let () = print_endline ""
let () = print_endline "*************************************"
let () = print_endline "********** Starting MTD! ************"
let () = print_endline "*************************************"
let () = print_endline ""




open Raylib


let setup () =
  Raylib.init_window 1100 720 "MTD";
  Raylib.set_target_fps 60;

  let title_font = Raylib.load_font_ex "machine-gunk.ttf" 100 None in

  
  let custom_font = Raylib.load_font_ex "machine-gunk.ttf" 24 None in
  Raygui.set_font custom_font;
  

  (*Create the intro screen art*)
  let intro_screen_art = Raylib.load_image "MTDCoverArt.png" in 
  let texture = Raylib.load_texture_from_image intro_screen_art in
  unload_image intro_screen_art;

  let red_balloon = Raylib.load_image "red.png" in
  let red_bal_texture = Raylib.load_texture_from_image red_balloon in 
  unload_image red_balloon;



  (texture, red_bal_texture, title_font)

let pos1 = ref 550.

let pos2 = ref 650.

let pos3 = ref 800. 

let pos4 = ref 775.

let pos5 = ref 900.

let pos6 = ref 1100.

let pos7 = ref 1120.

let pos8 = ref 1300. 

let pos9 = ref 1400.

let update_balloon_position(pos) = 
  if !pos < ~-.5. then pos:= 650. else pos:= !pos -. 1.; pos 

let rec loop (tuple) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
 
    
    match tuple with 
    | (a, b, c) -> let texture = a in
    let red_bal_texture = b in let title_font = c in

    (***** BACKGROUND *****)
    (draw_texture_ex texture       
    (Vector2.create 0. 0.0)  (* Position *)
    0.0                          (* Rotation (in radians) *)
    (0.60)                (* Scale *)
    Color.white);

    (* Raylib.set_texture_filter (Font.texture (Raylib.get_font_default ())) TextureFilter.Point; *)

    draw_text_ex (title_font) ("McGraw Tower") (Vector2.create 430. 140.) 100. 3. (Color.create 255 6 0 255);
    draw_text_ex (title_font) ("Defense") (Vector2.create 570. 250.) 100. 3. (Color.create 255 6 0 255);  

    Raygui.(set_style (TextBox `Text_alignment) TextAlignment.(to_int Center));
    (* SETTING STYLE TO RED - USE HEX*)
    Raygui.(set_style (Button `Base_color_normal) 0xFF000010);
    Raygui.(set_style (Button `Text_color_normal) 0xFFFFFF);
    Raygui.set_style (Default `Text_size) 24;



    Raygui.(set_style (Button `Border_width) 0);
    (* create -> x y width height*)
    if Raygui.( (button (Rectangle.create 550. 360. 120. 50.) "PLAY")) then
      let open MTD in
      Raylib.close_window ();
      Tracker.setup () |> Tracker.loop else ();

    

    (***** BALLOONS *****)
    (draw_texture_ex red_bal_texture       
    (Vector2.create (500.) !(update_balloon_position(pos1))) 
    0.0                       
    (0.15)              
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (823.) (!(update_balloon_position(pos2)))) 
    0.0                         
    (0.15)                
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (341.) (!(update_balloon_position(pos3))))  
    0.0                       
    (0.15)              
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (763.) (!(update_balloon_position(pos4))))  
    0.0                         
    (0.15)                
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (240.) (!(update_balloon_position(pos5)))) 
    0.0                         
    (0.15)           
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (521.) (!(update_balloon_position(pos6)))) 
    0.0                         
    (0.15)              
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (780.) (!(update_balloon_position(pos7))))  
    0.0                         
    (0.15)                
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (620.) (!(update_balloon_position(pos8))))  
    0.0                    
    (0.15)                
    Color.white);
    (draw_texture_ex red_bal_texture       
    (Vector2.create (250.) (!(update_balloon_position(pos9))))  
    0.0                    
    (0.15)                
    Color.white);

    end_drawing ();
    loop (tuple)

let () = setup () |> loop