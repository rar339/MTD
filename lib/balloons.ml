type colors =
  | None
  | Red of int
  | Blue of int
  | White of int
  | Black of int
  | Brown of int
  | Yellow of int
  | Lead of int

type balloon = {
  mutable color : colors;
  mutable velocity : Raylib.Vector2.t;
  mutable position : Raylib.Vector2.t;
  mutable next_down : colors;
  mutable is_lead : bool;
  mutable img : string;
  order : int;
}

let make_redb i = {
  color = Red 1;
  velocity = Raylib.Vector2.create 1.0 1.0;
  position = Raylib.Vector2.create 1.0 1.0;
  next_down = None;
  is_lead = false;
  img = "red.png";
  order = i;
  }

let make_blueb i = {
    color = Blue 2;
    velocity = Raylib.Vector2.create 1.5 1.5;
    position = Raylib.Vector2.create 1.0 1.0;
    next_down = Red 1;
    is_lead = false;
    img = "blue.png";
    order = i;
}

