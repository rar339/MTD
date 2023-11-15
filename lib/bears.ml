open Raylib
(* open Constants *)

type bear_types = Dart | Hockey | Pumpkin | Ezra | Dragon

(*Width and height are temporary, shouldn't be needed if all images are the same
   size.*)
type bear = {
  bear_type : bear_types;
  mutable range : float;
  mutable cost : int;
  mutable upgrades : int;
  is_bomb : bool;
  mutable position : Raylib.Vector2.t;
  texture : Raylib.Texture2D.t;
  image_width : float;
  image_height : float;
  is_placed : bool;
  mutable attack_speed : int;
  mutable counter : int;
  projectile_speed : float;
  mutable sold : bool;
  mutable damage : int;
}

let bear_collection : bear list ref = ref []
let bear_radius = 30.
let get_x bear = Vector2.x bear.position
let get_y bear = Vector2.y bear.position

(*The bears displayed on the menu.*)
let menu_bears : bear list ref = ref []

let string_of_beartype bear_type =
  match bear_type with
  | Dart -> "Dart"
  | Hockey -> "Hockey"
  | Pumpkin -> "Pumpkin"
  | Ezra -> "Ezra"
  | Dragon -> "Dragon"

let make_dart_bear pos =
  let image = load_image "./img/redbear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Dart;
    range = 150.;
    cost = 200;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 30;
    counter = 0;
    projectile_speed = 10.;
    sold = false;
    damage = 1;
  }

(******************************************************************************)
let make_hockey_bear pos =
  let image = load_image "./img/bluebear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Hockey;
    range = 90.;
    cost = 200;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 50;
    counter = 50;
    projectile_speed = 60.;
    sold = false;
    damage = 1;
  }

let make_pumpkin_bear pos =
  let image = load_image "./img/blackbear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Pumpkin;
    range = 120.;
    cost = 350;
    upgrades = 0;
    is_bomb = true;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
    sold = false;
    damage = 1;
  }

let make_ezra_bear pos =
  let image = load_image "./img/purplebear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Ezra;
    range = 80.;
    cost = 400;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
    sold = false;
    damage = 1;
  }

let make_dragon_bear pos =
  let image = load_image "./img/greenbear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Dragon;
    range = 120.;
    cost = 1000;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
    sold = false;
    damage = 1;
  }

let generate_menu_bears screen_width screen_height =
  [
    make_dart_bear
      (Vector2.create (5.45 *. screen_width /. 7.) (1. *. screen_height /. 4.));
    make_hockey_bear
      (Vector2.create (5.75 *. screen_width /. 7.) (1. *. screen_height /. 4.));
    make_pumpkin_bear
      (Vector2.create (6.05 *. screen_width /. 7.) (1. *. screen_height /. 4.));
    make_ezra_bear
      (Vector2.create (6.35 *. screen_width /. 7.) (1. *. screen_height /. 4.));
    make_dragon_bear
      (Vector2.create (6.65 *. screen_width /. 7.) (1. *. screen_height /. 4.));
  ]

(*Returns the bear clicked, if any.*)
let rec determine_bear_clicked click_pos bear_list =
  match bear_list with
  | [] -> None
  | bear :: rest ->
      if check_collision_point_circle click_pos bear.position bear_radius then
        Some bear
      else determine_bear_clicked click_pos rest

(* draw_bear function *)
let draw_bear (bear : bear) =
  let x = Vector2.x bear.position -. bear_radius in
  let y = Vector2.y bear.position -. bear_radius in
  draw_texture_pro bear.texture
    (*Source rect should be the size of the bear's img file.*)
    (Rectangle.create 0. 0. bear.image_width bear.image_height)
    (*Dest rect*)
    (Rectangle.create x y (bear_radius *. 2.) (bear_radius *. 2.))
    (Vector2.create 0. 0.) 0.
    (Color.create 255 255 255 255)

(*Draws the placed bears in the game*)
let rec draw_bears (bears : bear list) =
  match bears with
  | [] -> ()
  | bear :: rest ->
      draw_bear bear;
      draw_bears rest

let draw_selected_bear (bear : bear option) =
  match bear with None -> () | Some bear -> draw_bear bear

let check_circle_collision circ_one circ_two radius =
  Vector2.distance circ_one circ_two < 2. *. radius

let update_selected_bear (bear : bear option) (new_pos : Vector2.t) =
  match bear with None -> () | Some bear -> bear.position <- new_pos

let rec check_collision_bears (selected_bear : bear option)
    (placed_bears : bear list) =
  match placed_bears with
  | [] -> false
  | h :: t -> (
      match selected_bear with
      | None -> true
      | Some bear ->
          if check_circle_collision bear.position h.position bear_radius then
            true
          else check_collision_bears selected_bear t)

let rec remove_bears bear_lst =
  match bear_lst with
  | [] -> []
  | bear :: rest ->
      if bear.sold then remove_bears rest else bear :: remove_bears rest