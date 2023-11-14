(* This file specifies the properties and behaviors of balloons. There
   are several different colors of balloons, each with their own velocities
   and associated values. *)
open Raylib
open Bears

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
  | Lead -> 7

type balloon = {
  mutable color : balloon_colors;
  mutable velocity : Raylib.Vector2.t;
  mutable position : Raylib.Vector2.t;
  mutable next_down : balloon_colors;
  mutable is_lead : bool;
  mutable img : Raylib.Texture2D.t;
  mutable current_turn : int;
  mutable remove : bool;
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
    (Color.create 255 255 255 255)

(*Comment/uncomment the draw function below as needed for debugging hitbox.*)
(* draw_rectangle
   (Constants.round_float
      (Vector2.x balloon.position -. (!hitbox_width /. 2.)))
   (Constants.round_float
      (Vector2.y balloon.position -. (!hitbox_height /. 2.)))
   (Constants.round_float !hitbox_width)
   (Constants.round_float !hitbox_height)
   Color.gold; *)
(*Comment/uncomment the draw function below as needed for debugging hitbox.*)
(* draw_circle
   (round_float (x +. !hitbox_y_offset +. (!hitbox_width /. 2.)))
   (round_float (y +. !hitbox_y_offset +. (!hitbox_height /. 2.)))
   5.0 Color.green *)

(* Draws balloons in a balloon list. *)
let rec draw_balloons path_width (balloon_list : balloon list) =
  match balloon_list with
  | [] -> ()
  | h :: t ->
      draw_balloon path_width h;
      draw_balloons path_width t

let determine_image = function
  | None ->
      let balloon_image = Raylib.load_image "./img/red.png" in
      Raylib.load_texture_from_image balloon_image
  | Red ->
      let balloon_image = Raylib.load_image "./img/red.png" in
      Raylib.load_texture_from_image balloon_image
  | Blue ->
      let balloon_image = Raylib.load_image "./img/blue.png" in
      Raylib.load_texture_from_image balloon_image
  | Green ->
      let balloon_image = Raylib.load_image "./img/green.png" in
      Raylib.load_texture_from_image balloon_image
  | Yellow ->
      let balloon_image = Raylib.load_image "./img/yellow.png" in
      Raylib.load_texture_from_image balloon_image
  | Orange ->
      let balloon_image = Raylib.load_image "./img/orange.png" in
      Raylib.load_texture_from_image balloon_image
  | Purple ->
      let balloon_image = Raylib.load_image "./img/purple.png" in
      Raylib.load_texture_from_image balloon_image
  | Lead ->
      let balloon_image = Raylib.load_image "./img/lead.png" in
      Raylib.load_texture_from_image balloon_image

let determine_next = function
  | None -> None
  | Red -> None
  | Blue -> Red
  | Green -> Blue
  | Yellow -> Green
  | Orange -> Yellow
  | Purple -> Orange
  | Lead -> Purple

(** Determines the velocity associated with a color of a balloon. *)
let determine_velocity = function
  | Red -> 5.0
  | Blue -> 8.0
  | Green -> 10.0
  | Yellow -> 10.0
  | Orange -> 12.0
  | Purple -> 12.0
  | Lead -> 5.0
  | _ -> 0.0

  (* Changes the velocity of a balloon while preserving its direction. *)
let change_velocity velocity next_down =
  if Vector2.x velocity = 0.0 then
    if Vector2.y velocity >= 0.0 then
      Vector2.create 0.0 (determine_velocity next_down)
    else Vector2.create 0.0 (-1.0 *. determine_velocity next_down)
  else if Vector2.x velocity >= 0.0 then
    Vector2.create (determine_velocity next_down) 0.0
  else Vector2.create (-1.0 *. determine_velocity next_down) 0.0

(* Creates a balloon given the position and color. *)
let make_balloon i position color is_lead =
  let x = Vector2.x position in
  let y = Vector2.y position in
  {
    color;
    velocity = Raylib.Vector2.create (determine_velocity color) 0.0;
    position =
      Vector2.create
        (x +. !hitbox_y_offset +. (!hitbox_width /. 2.))
        (y +. !hitbox_y_offset +. (!hitbox_height /. 2.));
    next_down = determine_next color;
    is_lead;
    img = determine_image color;
    current_turn = 0;
    remove = false;
    order = i;
  }

(*Lowers player lives when a balloon crosses the finish line based on the
   value of that balloon. *)
let lower_lives balloon = Constants.(lives := !lives - balloon_value balloon)

(*Checks if a balloon has reached the finish line. *)
let check_balloon_exit (balloon : balloon) =
  let y = Vector2.y balloon.position in
  if y < Constants.end_line then (
    lower_lives balloon.color;
    true)
  else false

(** Modifies the given balloon to be one layer color 'lower'. If the balloon
   is red, sets balloon.remove to true.*)
let lower_layer balloon =
  match balloon.color with
  | Red -> balloon.remove <- true
  | _ ->
      balloon.color <- balloon.next_down;
      balloon.velocity <- change_velocity balloon.velocity balloon.next_down;
      balloon.img <- determine_image balloon.color;
      balloon.next_down <- determine_next balloon.color

(** Modifies lead balloon such that if hit by dragon, no longer a lead ballooon *)
let remove_lead balloon = balloon.is_lead <- false

(** Modifies the given balloon to be the correct layer color based on the damage
   of the projectile. If lead ballon hit and not dragon, do not modify balloon.*)
let rec hit_update (bear : bear_types) damage balloon =
  if damage = 0 || balloon.remove then ()
  else if balloon.is_lead then
    match bear with
    | Dragon ->
        remove_lead balloon;
        lower_layer balloon;
        Constants.cash := !Constants.cash + 1;
        hit_update bear (damage - 1) balloon
    | _ -> hit_update bear (damage - 1) balloon
  else (
    lower_layer balloon;
    Constants.cash := !Constants.cash + 1;
    hit_update bear (damage - 1) balloon)

(* Removes a balloon if it has crossed the finish line and reduces a player's
   lives by calling lower_lives. *)
let rec remove_balloons (balloon_lst : balloon list) =
  match balloon_lst with
  | [] -> []
  | balloon :: t ->
      if check_balloon_exit balloon || balloon.remove then remove_balloons t
      else balloon :: remove_balloons t
