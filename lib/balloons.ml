type colors =
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
    mutable is_lead : bool;
    mutable img : string;
  }

  let make_redb () = {
      color = Red 1;
      velocity = Raylib.Vector2.create 1.0 1.0;
      position = Raylib.Vector2.create 1.0 1.0;
      is_lead = false;
      img = "red.png";
  }

  let make_blueb() = {
    color = Blue 1;
    velocity = Raylib.Vector2.create 1.5 1.5;
    position = Raylib.Vector2.create 1.0 1.0;
    is_lead = false;
    img = "blue.png";
  }



