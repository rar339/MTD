open Raylib
open Constants

let background : Texture2D.t option ref = ref None
let background_width : int ref = ref 0
let background_height : int ref = ref 0

let draw_background (background_art : Texture2D.t) =
  draw_texture_pro background_art
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

(*Home screen balloons**********************************************************)
  let balloon_h_radius = 20.
  let balloon_v_radius = 25.

  type balloon = {
    x : float;
    mutable y : float;
    speed : float;
    img : Texture2D.t;
  }

  let gen_balloon x y speed img = { x; y; speed; img }

  let rec generate_all_balloons x_pos_start count img : balloon list =
    if x_pos_start < int_of_float (!Constants.screen_width -. 100.) then
      let rand_x = Random.float 300. in
      let rand_y = Random.float 200. in
      let rand_speed = Random.float 1. in
      gen_balloon
        (float_of_int x_pos_start +. rand_x)
        (!Constants.screen_height +. rand_y)
        (rand_speed +. 1.) img
      :: generate_all_balloons
           (x_pos_start + int_of_float rand_x)
           (count - 1) img
    else []

  let update_balloon_position balloon =
    let new_y =
      if balloon.y < -70. then !screen_height +. 70.
      else balloon.y -. balloon.speed
    in
    balloon.y <- new_y

  let rec update_balloon_positions (balloons : balloon list) =
    match balloons with
    | [] -> ()
    | h :: t ->
        update_balloon_position h;
        update_balloon_positions t

  let draw_balloon balloon =
    draw_texture_ex balloon.img
      (Vector2.create balloon.x balloon.y)
      0.0 0.15 Color.white

  let rec draw_balloons (balloons : balloon list) =
    match balloons with
    | [] -> ()
    | h :: t ->
        draw_balloon h;
        draw_balloons t

  let check_clicked balloon (click_pos : Vector2.t) : bool =
    (*Magic numbers represent the offset from the top left corner of PNG to
       actual center of balloon ellipse.*)
    let bal_x = balloon.x +. 29.0 in
    let bal_y = balloon.y +. 35.0 in
    if
      Vector2.x click_pos < bal_x +. balloon_h_radius
      && Vector2.x click_pos > bal_x -. balloon_h_radius
      && Vector2.y click_pos < bal_y +. balloon_v_radius
      && Vector2.y click_pos > bal_y -. balloon_v_radius
    then true
    else false

  let rec check_clicked_all_balloons balloons (click_pos : Vector2.t) :
      balloon list =
    match balloons with
    | [] -> []
    | balloon :: rest ->
        if check_clicked balloon click_pos then
          check_clicked_all_balloons rest click_pos
        else balloon :: check_clicked_all_balloons rest click_pos

