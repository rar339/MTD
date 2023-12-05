(* This file specifies the properties and behaviors of balloons. There
   are several different colors of balloons, each with their own velocities
   and associated values. *)
open Raylib
open Bears
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
let value_of_balloon = function
  | None -> 0
  | Red -> 1
  | Blue -> 2
  | Green -> 3
  | Yellow -> 4
  | Orange -> 5
  | Purple -> 6
  | Lead -> 7

let balloon_of_value = function
  | 1 -> Red
  | 2 -> Blue
  | 3 -> Green
  | 4 -> Yellow
  | 5 -> Orange
  | 6 -> Purple
  | 7 -> Lead
  | _ -> None

type balloon = {
  mutable color : balloon_colors;
  mutable velocity : Raylib.Vector2.t;
  mutable position : Raylib.Vector2.t;
  mutable is_lead : bool;
  mutable img : Raylib.Texture2D.t;
  mutable current_turn : int;
  mutable remove : bool;
  mutable is_slowed : bool;
  mutable slow_counter : int;
  order : int; (*We likely do not need this*)
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

(* Draws pop image n times when called *)
let rec draw_pop balloon n =
  if n = 0 then ()
  else (
    draw_texture_pro (Option.get !pop_img)
      (Rectangle.create 0. 0. 146. 120.)
      (Rectangle.create
         (Vector2.x balloon.position -. 32.5)
         (Vector2.y balloon.position -. 50.0)
         80. 80.)
      (Vector2.create 0. 0.) 0.
      (Color.create 255 255 255 255);
    draw_pop balloon (n - 1))

(* Draws balloons in a balloon list. *)
let rec draw_balloons path_width (balloon_list : balloon list) =
  match balloon_list with
  | [] -> ()
  | h :: t ->
      draw_balloon path_width h;
      draw_balloons path_width t

let determine_image balloon_type =
  match balloon_type with
  | None -> failwith "impossible?"
  | Red -> Raylib.load_texture "./img/red.png"
  | Blue -> Raylib.load_texture "./img/blue.png"
  | Green -> Raylib.load_texture "./img/green.png"
  | Yellow -> Raylib.load_texture "./img/yellow.png"
  | Orange -> Raylib.load_texture "./img/orange.png"
  | Purple -> Raylib.load_texture "./img/purple.png"
  | Lead -> Raylib.load_texture "./img/lead.png"

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
let change_velocity balloon new_color =
  let velocity = balloon.velocity in
  if Vector2.x velocity = 0.0 then
    if Vector2.y velocity >= 0.0 then
      balloon.velocity <- Vector2.create 0.0 (determine_velocity new_color)
    else
      balloon.velocity <-
        Vector2.create 0.0 (-1.0 *. determine_velocity new_color)
  else if Vector2.x velocity >= 0.0 then
    balloon.velocity <- Vector2.create (determine_velocity new_color) 0.0
  else
    balloon.velocity <-
      Vector2.create (-1.0 *. determine_velocity new_color) 0.0

(* Creates a balloon given the color. *)
let make_balloon i color is_lead =
  let position =
    Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.))
  in
  let x = Vector2.x position in
  let y = Vector2.y position in
  {
    color;
    velocity = Raylib.Vector2.create (determine_velocity color) 0.0;
    position =
      Vector2.create
        (x +. !hitbox_y_offset +. (!hitbox_width /. 2.))
        (y +. !hitbox_y_offset +. (!hitbox_height /. 2.));
    is_lead;
    img = determine_image color;
    current_turn = 0;
    is_slowed = false;
    remove = false;
    order = i;
    slow_counter = 180;
  }

(*Lowers player lives when a balloon crosses the finish line based on the
   value of that balloon. *)
let lower_lives balloon = Constants.(lives := !lives - value_of_balloon balloon)

(*Checks if a balloon has reached the finish line. *)
let check_balloon_exit (balloon : balloon) =
  let y = Vector2.y balloon.position in
  if y < Constants.end_line then (
    lower_lives balloon.color;
    true)
  else false

let set_balloon_color balloon new_color =
  balloon.color <- new_color;
  balloon.img <- determine_image new_color;
  change_velocity balloon new_color;
  if new_color <> Lead then balloon.is_lead <- false

(**Updates a balloons color, etc. after a collision with a projectile.
    If bear is a zombie, slows down the balloon*)
let update_balloon_status bear balloon =
  (if bear.bear_type == Zombie && not balloon.is_slowed then
     let new_velocity =
       Vector2.multiply (Vector2.create 0.5 0.5) balloon.velocity
     in
     balloon.velocity <- new_velocity);

  if (balloon.is_lead && bear.pops_lead) || not balloon.is_lead then
    let new_value = value_of_balloon balloon.color - bear.damage in
    match balloon_of_value new_value with
    (*If none, the balloon should be removed.*)
    | None ->
        balloon.remove <- true;
        Constants.cash := !Constants.cash + value_of_balloon balloon.color;
        begin_drawing ();
        draw_pop balloon (value_of_balloon balloon.color);
        end_drawing ()
    | color ->
        set_balloon_color balloon color;
        Constants.cash := !Constants.cash + new_value;
        begin_drawing ();
        draw_pop balloon bear.damage;
        end_drawing ()

(** Modifies the given balloon to be the correct layer color based on the damage
   of the bear. If lead ballon hit and not able to pop lead, do not modify balloon.*)
let hit_update (bear : bear) (balloon : balloon) =
  if balloon.remove then () else update_balloon_status bear balloon

(* Removes a balloon if it has crossed the finish line and reduces a player's
   lives by calling lower_lives. *)
let rec remove_balloons (balloon_lst : balloon list) =
  match balloon_lst with
  | [] -> []
  | balloon :: t ->
      if check_balloon_exit balloon || balloon.remove then remove_balloons t
      else balloon :: remove_balloons t
