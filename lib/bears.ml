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
}

let bear_radius = 30.
let reference_img_radius = bear_radius
let get_x bear = Vector2.x bear.position
let get_y bear = Vector2.y bear.position

(*DART BEARS*******************************************************************)

let draw_dart_bear_img x y =
  draw_circle (int_of_float x) (int_of_float y) reference_img_radius Color.red

let make_dart_bear pos =
  {
    bear_type = Dart;
    range = 120.;
    cost = 200;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
  }

let draw_dart_bear (bear : bear) =
  draw_circle
    (int_of_float (Vector2.x bear.position))
    (int_of_float (Vector2.y bear.position))
    (bear_radius) Color.red

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
  }

(*Reference Bear Helpers!*)
let determine_ref_bear_clicked (click_pos : Vector2.t) (screen_w : float)
    (screen_h : float) =
  if
    Vector2.x click_pos <= (6. *. screen_w /. 7.) +. 70.
    && Vector2.x click_pos >= (6. *. screen_w /. 7.) -. 70.
    && Vector2.y click_pos <= (1. *. screen_h /. 4.) +. 70.
    && Vector2.y click_pos >= (1. *. screen_h /. 4.) -. 70.
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
  | Some bear -> match bear with
    | { bear_type = Dart; _ } -> draw_dart_bear_img (get_x bear) (get_y bear);
    | { bear_type = Hockey; _ } -> ()
    | { bear_type = Pumpkin; _ } -> ()
    | { bear_type = Dragon; _ } -> ()
    | { bear_type = Ezra; _ } -> ()


let update_selected_bear (bear : bear option) (new_pos : Vector2.t) =
  match bear with
  | None -> ();
  | Some bear -> bear.position <- new_pos;