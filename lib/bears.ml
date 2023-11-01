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
  position : Raylib.Vector2.t;
  img : string;
  is_placed : bool;
}

let reference_img_radius = 50.

(*DART BEARS*******************************************************************)
let draw_dart_bear_img width height =
  draw_circle
    (int_of_float ( 6. *. width /. 7.))
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

