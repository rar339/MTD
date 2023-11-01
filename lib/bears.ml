type bear = {
  name : string;
  mutable range : float;
  cost : int;
  mutable rate : Raylib.Vector2.t;
  radius : float;
  upgrades : int list;
  is_bomb : bool;
  position : Raylib.Vector2.t;
  img : Raylib.Image.t;
}
