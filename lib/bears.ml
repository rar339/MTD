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
}
