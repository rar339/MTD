open Raylib
(* open Constants *)

type bear_types = Dart | Hockey | Pumpkin | Ezra | Dragon

type bear = {
  bear_type : bear_types;
  mutable range : float;
  cost : int;
  mutable rate : Raylib.Vector2.t;
  radius : float;
  upgrades : int list;
  is_bomb : bool;
  mutable position : Raylib.Vector2.t;
  img : string;
  is_placed : bool;
  attack_speed : int;
  mutable counter : int;
  projectile_speed : float;
}

let bear_collection : bear list ref = ref []
let bear_radius = 30.
let reference_img_radius = bear_radius /. 2.
let get_x bear = Vector2.x bear.position
let get_y bear = Vector2.y bear.position

(* Bear-Drawers *)

let draw_bear_img x y color =
  draw_circle (int_of_float x) (int_of_float y) reference_img_radius color;
  draw_ring_lines (Vector2.create x y)
    (reference_img_radius /. 1.2)
    reference_img_radius 0. 0. 10 Color.black

let make_dart_bear pos =
  {
    bear_type = Dart;
    range = 250.;
    cost = 200;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
    attack_speed = 10;
    counter = 0;
    projectile_speed = 10.;
  }

let draw_dart_bear (bear : bear) =
  draw_circle
    (int_of_float (Vector2.x bear.position))
    (int_of_float (Vector2.y bear.position))
    bear_radius Color.red

(******************************************************************************)
let make_hockey_bear pos =
  {
    bear_type = Hockey;
    range = 80.;
    cost = 200;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
  }

let make_pumpkin_bear pos =
  {
    bear_type = Dart;
    range = 80.;
    cost = 350;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
  }

let make_ezra_bear pos =
  {
    bear_type = Dart;
    range = 80.;
    cost = 400;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
  }

let make_dragon_bear pos =
  {
    bear_type = Dart;
    range = 120.;
    cost = 1000;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
  }

(*Reference Bear Helpers!*)
let determine_dart_bear_clicked (click_pos : Vector2.t) (screen_w : float)
    (screen_h : float) =
  if
    Vector2.x click_pos <= (5.45 *. screen_w /. 7.) +. 35.
    && Vector2.x click_pos >= (5.45 *. screen_w /. 7.) -. 35.
    && Vector2.y click_pos <= (1. *. screen_h /. 4.) +. 35.
    && Vector2.y click_pos >= (1. *. screen_h /. 4.) -. 35.
  then true
  else false

let determine_hockey_bear_clicked (click_pos : Vector2.t) (screen_w : float)
  (screen_h : float) =
if
  Vector2.x click_pos <= (5.75 *. screen_w /. 7.) +. 35.
  && Vector2.x click_pos >= (5.75 *. screen_w /. 7.) -. 35.
  && Vector2.y click_pos <= (1. *. screen_h /. 4.) +. 35.
  && Vector2.y click_pos >= (1. *. screen_h /. 4.) -. 35.
then true
else false

let determine_pumpkin_bear_clicked (click_pos : Vector2.t) (screen_w : float)
  (screen_h : float) =
if
  Vector2.x click_pos <= (6.05 *. screen_w /. 7.) +. 35.
  && Vector2.x click_pos >= (6.05 *. screen_w /. 7.) -. 35.
  && Vector2.y click_pos <= (1. *. screen_h /. 4.) +. 35.
  && Vector2.y click_pos >= (1. *. screen_h /. 4.) -. 35.
then true
else false

let determine_ezra_bear_clicked (click_pos : Vector2.t) (screen_w : float)
  (screen_h : float) =
if
  Vector2.x click_pos <= (6.35 *. screen_w /. 7.) +. 35.
  && Vector2.x click_pos >= (6.35 *. screen_w /. 7.) -. 35.
  && Vector2.y click_pos <= (1. *. screen_h /. 4.) +. 35.
  && Vector2.y click_pos >= (1. *. screen_h /. 4.) -. 35.
then true
else false

let determine_dragon_bear_clicked (click_pos : Vector2.t) (screen_w : float)
  (screen_h : float) =
if
  Vector2.x click_pos <= (5.45 *. screen_w /. 7.) +. 35.
  && Vector2.x click_pos >= (5.45 *. screen_w /. 7.) -. 35.
  && Vector2.y click_pos <= (1. *. screen_h /. 4.) +. 35.
  && Vector2.y click_pos >= (1. *. screen_h /. 4.) -. 35.
then true
else false



(*Draws the placed bears in the game*)
let rec draw_bears (bears : bear list) =
  match bears with
  | [] -> ()
  | bear :: rest -> (
      match bear with
      | { bear_type = Dart; _ } ->
          draw_dart_bear bear;
          draw_bears rest
      | { bear_type = Hockey; _ } ->
          print_endline "Drawing a Hockey bear";
          draw_bears rest
      | { bear_type = Pumpkin; _ } ->
          print_endline "Drawing a Pumpkin bear";
          draw_bears rest
      | { bear_type = Dragon; _ } ->
          print_endline "Drawing a Dragon bear";
          draw_bears rest
      | { bear_type = Ezra; _ } ->
          print_endline "Drawing an Ezra bear";
          draw_bears rest)

let draw_selected_bear (bear : bear option) =
  match bear with
  | None -> ()
  | Some bear -> (
      match bear with
      | { bear_type = Dart; _ } ->
          draw_bear_img (get_x bear) (get_y bear) Color.red
      | { bear_type = Hockey; _ } -> ()
      | { bear_type = Pumpkin; _ } -> ()
      | { bear_type = Dragon; _ } -> ()
      | { bear_type = Ezra; _ } -> ())

let check_circle_collision circ_one circ_two radius =
  if Vector2.distance circ_one circ_two < 2. *. radius then true else false

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
