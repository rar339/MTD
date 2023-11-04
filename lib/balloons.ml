(* This file specifies the properties and behaviors of balloons. There
   are several different colors of balloons, each with their own velocities
   and associated values. *)
open Raylib
open Constants

type balloon_colors =
  | None
  | Red
  | Blue
  | Green
  | Orange
  | Purple
  | Yellow
  | Lead

(* Maps a balloon to an integer value. *)
let balloon_value = function
  | None -> 0
  | Red -> 1
  | Blue -> 2
  | Green -> 3
  | Yellow -> 4
  | Orange -> 5
  | Purple -> 6
  | _ -> 7

type balloon = {
  mutable color : balloon_colors;
  mutable velocity : Raylib.Vector2.t;
  mutable position : Raylib.Vector2.t;
  mutable next_down : balloon_colors;
  mutable is_lead : bool;
  mutable img : Raylib.Texture2D.t;
  mutable current_turn : int;
  order : int;
}

let hitbox_width = ref 0.0
let hitbox_height = ref 0.0
let hitbox_x_offset = ref 0.0
let hitbox_y_offset = ref 0.0

let setup_hitbox path_width =
  hitbox_width := path_width /. 1.5;
  hitbox_height := path_width /. 1.5;
  hitbox_x_offset := path_width *. 0.21;
  hitbox_y_offset := path_width *. 0.21

(* This should be dependent on the size of the balloon image. Needs fine tuning. *)
let get_hitbox (balloon : balloon) =
  Rectangle.create
    (Vector2.x balloon.position -. (!hitbox_width /. 2.))
    (Vector2.y balloon.position -. (!hitbox_height /. 2.))
    !hitbox_width !hitbox_height

let draw_balloon path_width (balloon : balloon) =
  let x =
    Vector2.x balloon.position -. !hitbox_y_offset -. (!hitbox_width /. 2.)
  in
  let y =
    Vector2.y balloon.position -. !hitbox_y_offset -. (!hitbox_width /. 2.)
  in
  draw_texture_pro balloon.img
    (Rectangle.create 0. 0. 385. 500.)
    (Rectangle.create x y 80. path_width)
    (Vector2.create 0. 0.) 0.
    (Color.create 255 255 255 255);

  (*Comment/uncomment the draw function below as needed for debugging hitbox.*)
  draw_rectangle
    (Constants.round_float
       (Vector2.x balloon.position -. (!hitbox_width /. 2.)))
    (Constants.round_float
       (Vector2.y balloon.position -. (!hitbox_height /. 2.)))
    (Constants.round_float !hitbox_width)
    (Constants.round_float !hitbox_height)
    Color.gold;
  (*Comment/uncomment the draw function below as needed for debugging hitbox.*)
  draw_circle
    (round_float (x +. !hitbox_y_offset +. (!hitbox_width /. 2.)))
    (round_float (y +. !hitbox_y_offset +. (!hitbox_height /. 2.)))
    5.0 Color.green

(* Draws balloons in a balloon list. *)
let rec draw_balloons path_width (balloon_list : balloon list) =
  match balloon_list with
  | [] -> ()
  | h :: t ->
      draw_balloon path_width h;
      draw_balloons path_width t

(* Creates a red balloon. *)
let make_redb i position =
  let x = Vector2.x position in
  let y = Vector2.y position in
  {
    color = Red;
    velocity = Raylib.Vector2.create 5.0 0.0;
    position =
      Vector2.create
        (x +. !hitbox_y_offset +. (!hitbox_width /. 2.))
        (y +. !hitbox_y_offset +. (!hitbox_height /. 2.));
    next_down = None;
    is_lead = false;
    img =
      (let balloon_image = Raylib.load_image "./img/red.png" in
       Raylib.load_texture_from_image balloon_image);
    current_turn = 0;
    order = i;
  }

(* Creates a blue balloon *)
let make_blueb i position =
  {
    color = Blue;
    velocity = Raylib.Vector2.create 15.0 0.0;
    position;
    next_down = Red;
    is_lead = false;
    img =
      (let balloon_image = Raylib.load_image "./img/blue.png" in
       Raylib.load_texture_from_image balloon_image);
    current_turn = 0;
    order = i;
  }

(* Checks if a balloon has reached the finish line. *)
let check_balloon_exit (balloon : balloon) =
  let y = Vector2.y balloon.position in
  if y < Constants.end_line then true else false

(* Lowers player lives when a balloon crosses the finish line based on the
   value of that balloon. *)
let lower_lives balloon = Constants.(lives := !lives - balloon_value balloon)

(* Removes a balloon if it has crossed the finish line and reduces a player's
   lives by calling lower_lives. *)
let rec remove_out_of_bounds (balloon_lst : balloon list) =
  match balloon_lst with
  | [] -> []
  | h :: t ->
      if check_balloon_exit h then (
        lower_lives h.color;
        remove_out_of_bounds t)
      else h :: remove_out_of_bounds t
