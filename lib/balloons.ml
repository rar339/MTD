open Raylib

type balloon_colors =
  | None
  | Red of int
  | Blue of int
  | White of int
  | Black of int
  | Brown of int
  | Yellow of int
  | Lead of int

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

let get_hitbox (balloon : balloon) =
  Rectangle.create
    (Vector2.x balloon.position +. 10.)
    (Vector2.y balloon.position +. 10.)
    40.0 50.0

let draw_balloon (balloon : balloon) =
  let x = Vector2.x balloon.position in
  let y = Vector2.y balloon.position in
  draw_texture_ex balloon.img (Vector2.create x y) 0.0 0.15 Color.white

let rec draw_balloons (balloon_list : balloon list) =
  match balloon_list with
  | [] -> ()
  | h :: t ->
      draw_balloon h;
      draw_balloons t

let make_redb i position =
  {
    color = Red 1;
    velocity = Raylib.Vector2.create 3.0 0.0;
    position;
    next_down = None;
    is_lead = false;
    img =
      (let balloon_image = Raylib.load_image "red.png" in
       Raylib.load_texture_from_image balloon_image);
    current_turn = 0;
    order = i;
  }

let make_blueb i position =
  {
    color = Blue 2;
    velocity = Raylib.Vector2.create 5.0 0.0;
    position;
    next_down = Red 1;
    is_lead = false;
    img =
      (let balloon_image = Raylib.load_image "blue.png" in
       Raylib.load_texture_from_image balloon_image);
    current_turn = 0;
    order = i;
  }
