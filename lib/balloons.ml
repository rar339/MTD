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

(*Balloon drawing functions****************************************************)
(*This should be dependent on the size of the balloon image. Needs fine tuning.*)
let get_hitbox path_width (balloon : balloon) =
  Rectangle.create
    (Vector2.x balloon.position +. (path_width *. 0.21))
    (Vector2.y balloon.position +. (path_width *. 0.21))
    (path_width /. 1.5) (path_width /. 1.5)

let draw_balloon path_width (balloon : balloon) =
  let x = Vector2.x balloon.position in
  let y = Vector2.y balloon.position in
  draw_texture_pro balloon.img
    (Rectangle.create 0. 0. 375. 500.)
    (Rectangle.create x y 80. path_width)
    (Vector2.create 0. 0.) 0.
    (Color.create 255 255 255 255)
  (*Comment/uncomment the draw function below as needed for debugging hitbox*)
  (* draw_rectangle
    (Constants.round_float (x +. (path_width *. 0.21)))
    (Constants.round_float (y +. (path_width *. 0.21)))
    (Constants.round_float (path_width /. 1.5))
    (Constants.round_float (path_width /. 1.5))
    Color.gold *)

(* draw_texture_ex balloon.img (Vector2.create x y) 0.0 0.15 Color.white *)

let rec draw_balloons path_width (balloon_list : balloon list) =
  match balloon_list with
  | [] -> ()
  | h :: t ->
      draw_balloon path_width h;
      draw_balloons path_width t

let make_redb i position =
  {
    color = Red 1;
    velocity = Raylib.Vector2.create 3.0 0.0;
    position;
    next_down = None;
    is_lead = false;
    img =
      (let balloon_image = Raylib.load_image "./img/red.png" in
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
      (let balloon_image = Raylib.load_image "./img/yellow.png" in
       Raylib.load_texture_from_image balloon_image);
    current_turn = 0;
    order = i;
  }

(*Balloon list updating functions**********************************************)
let check_balloon_exit (balloon : balloon) =
  let y = Vector2.y balloon.position in
  if y < Constants.end_line then true else false

let rec remove_out_of_bounds (balloon_lst : balloon list) =
  match balloon_lst with
  | [] -> []
  | h :: t ->
      if check_balloon_exit h then remove_out_of_bounds t
      else h :: remove_out_of_bounds t
