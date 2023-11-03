open Raylib
(* open Constants *)

type bear_types = Dart | Hockey | Pumpkin | Ezra | Martha

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
let reference_img_radius = 50.

(*DART BEARS*******************************************************************)

let draw_dart_bear_img width height =
  draw_circle
    (int_of_float (6. *. width /. 7.))
    (int_of_float (1. *. height /. 4.))
    reference_img_radius Color.red

let make_dart_bear pos =
  {
    bear_type = Dart;
    range = 30.;
    cost = 10;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
  }

let draw_dart_bear (bear : bear)  =
  draw_circle (int_of_float(Vector2.x bear.position)) (int_of_float(Vector2.y bear.position)) 25. Color.red

(******************************************************************************)
let make_hockey_bear pos =
  {
    bear_type = Hockey;
    range = 30.;
    cost = 10;
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
    range = 30.;
    cost = 10;
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
    range = 30.;
    cost = 10;
    radius = 20.;
    rate = Vector2.create 0. 0.;
    upgrades = [];
    is_bomb = false;
    position = pos;
    img = "YO";
    is_placed = true;
  }

let make_martha_bear pos =
  {
    bear_type = Dart;
    range = 30.;
    cost = 10;
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
    && Vector2.x click_pos >= (6. *. screen_w /. 7.) -. +. 70.
    && Vector2.y click_pos <= (1. *. screen_h /. 4.) +. 70.
    && Vector2.y click_pos >= (1. *. screen_h /. 4.) -. 70.
  then true
  else false

(*Draws the placed bears in the game*)
let rec draw_bears (bears : bear list)  =
  match bears with
  | [] -> ()
  | bear :: rest -> (
      match bear with
      | { bear_type = Dart; _ } ->
          draw_dart_bear bear ;
          draw_bears rest
      | { bear_type = Hockey; _ } ->
          print_endline "Drawing a Hockey bear";
          draw_bears rest
      | { bear_type = Pumpkin; _ } ->
          print_endline "Drawing a Pumpkin bear";
          draw_bears rest
      | { bear_type = Martha; _ } ->
          print_endline "Drawing a Martha bear";
          draw_bears rest
      | { bear_type = Ezra; _ } ->
          print_endline "Drawing an Ezra bear";
          draw_bears rest)

