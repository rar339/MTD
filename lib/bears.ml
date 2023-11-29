open Raylib
(* open Constants *)

type bear_types = Dart | Hockey | Pumpkin | Sniper | Dragon

(*Width and height are temporary, shouldn't be needed if all images are the same
   size.*)
type bear = {
  bear_type : bear_types;
  mutable range : float;
  mutable cost : int;
  mutable upgrades : int;
  is_bomb : bool;
  mutable position : Raylib.Vector2.t;
  texture : Raylib.Texture2D.t;
  image_width : float;
  image_height : float;
  is_placed : bool;
  mutable attack_speed : int;
  mutable counter : int;
  projectile_speed : float;
  mutable sold : bool;
  mutable damage : int;
  mutable facing : float;
  mutable pops_lead : bool;
}

let bear_collection : bear list ref = ref []
let bear_radius = 40.
let menu_bear_radius = 52.
let get_x bear = Vector2.x bear.position
let get_y bear = Vector2.y bear.position

(*The bears displayed on the menu.*)
let menu_bears : bear list ref = ref []

let string_of_beartype bear_type =
  match bear_type with
  | Dart -> "Dart"
  | Hockey -> "Hockey"
  | Pumpkin -> "Pumpkin"
  | Sniper -> "Sniper"
  | Dragon -> "Dragon"

let make_dart_bear (menu_bear : bool) pos =
  let image =
    if not menu_bear then load_image "./img/newdart_bear.png"
    else load_image "./img/menu_dartbear.png"
  in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Dart;
    range = 150.;
    cost = 200;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 30;
    counter = 0;
    projectile_speed = 12.1;
    sold = false;
    damage = 1;
    pops_lead = false;
    facing = 0.;
  }

(******************************************************************************)
let make_hockey_bear (menu_bear : bool) pos =
  let image =
    if not menu_bear then load_image "./img/hockeybear.png"
    else load_image "./img/menu_hockeybear.png"
  in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Hockey;
    range = 90.;
    cost = 200;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 50;
    counter = 50;
    projectile_speed = 60.;
    sold = false;
    damage = 1;
    pops_lead = false;
    facing = 0.;
  }

let make_pumpkin_bear pos =
  let image = load_image "./img/blackbear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Pumpkin;
    range = 120.;
    cost = 350;
    upgrades = 0;
    is_bomb = true;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 80;
    counter = 0;
    projectile_speed = 10.;
    sold = false;
    damage = 1;
    pops_lead = false;
    facing = 0.;
  }

let make_sniper_bear pos =
  let image = load_image "./img/purplebear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Sniper;
    range = 1000.;
    cost = 400;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 150;
    counter = 50;
    projectile_speed = 30.;
    sold = false;
    damage = 100;
    pops_lead = true;
    facing = 0.;
  }

let make_dragon_bear pos =
  let image = load_image "./img/greenbear.png" in
  let image_width = float_of_int (Image.width image) in
  let image_height = float_of_int (Image.height image) in
  {
    bear_type = Dragon;
    range = 120.;
    cost = 1000;
    upgrades = 0;
    is_bomb = false;
    position = pos;
    texture = load_texture_from_image image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed = 10;
    counter = 50;
    projectile_speed = 10.;
    sold = false;
    damage = 1;
    pops_lead = true;
    facing = 0.;
  }

let generate_menu_bears screen_width screen_height =
  [
    make_dart_bear true
      (Vector2.create (5.55 *. screen_width /. 7.) (0.8 *. screen_height /. 4.));
    make_hockey_bear true
      (Vector2.create (6. *. screen_width /. 7.) (0.8 *. screen_height /. 4.));
    make_pumpkin_bear
      (Vector2.create (6.45 *. screen_width /. 7.) (0.8 *. screen_height /. 4.));
    make_sniper_bear
      (Vector2.create (5.75 *. screen_width /. 7.) (1.2 *. screen_height /. 4.));
    make_dragon_bear
      (Vector2.create (6.25 *. screen_width /. 7.) (1.2 *. screen_height /. 4.));
  ]

(*Returns the bear clicked, if any.*)
let rec determine_bear_clicked click_pos bear_list =
  match bear_list with
  | [] -> None
  | bear :: rest ->
      if check_collision_point_circle click_pos bear.position bear_radius then
        Some bear
      else determine_bear_clicked click_pos rest

let draw_menu_bear (bear : bear) =
  let x = Vector2.x bear.position in
  let y = Vector2.y bear.position in
  draw_texture_pro bear.texture
    (*Source rect should be the size of the bear's img file.*)
    (Rectangle.create 0. 0.
       (float_of_int (Texture2D.width bear.texture))
       (float_of_int (Texture2D.height bear.texture)))
    (*Dest rect*)
    (Rectangle.create x y (menu_bear_radius *. 2.) (menu_bear_radius *. 2.))
    (Vector2.create menu_bear_radius bear_radius)
    0.
    (Color.create 255 255 255 255)

(* Helps with resizing of images to make sure all bears are similar size*)
let dest_rect_custom (bear : bear) (x : float) (y : float) =
  match bear.bear_type with
  | Dart -> Rectangle.create x y (bear_radius *. 3.) (bear_radius *. 3.)
  | Hockey -> Rectangle.create x y (bear_radius *. 3.2) (bear_radius *. 3.2)
  | _ -> Rectangle.create x y (bear_radius *. 3.) (bear_radius *. 3.)

(* Helps with placement of images to ensure its centered, the factor must be
   1/2 of factor for dest_rect!*)
let origin_vect_custom (bear : bear) =
  match bear.bear_type with
  | Dart -> Vector2.create (bear_radius *. 1.5) (bear_radius *. 1.5)
  | Hockey -> Vector2.create (bear_radius *. 1.6) (bear_radius *. 1.6)
  | _ -> Vector2.create (bear_radius *. 1.5) (bear_radius *. 1.5)

(* draw_bear function *)
let draw_bear (bear : bear) =
  let x = Vector2.x bear.position in
  let y = Vector2.y bear.position in

  draw_texture_pro bear.texture
    (*Source rect should be the size of the bear's img file.*)
    (Rectangle.create 0. 0.
       (float_of_int (Texture2D.width bear.texture))
       (float_of_int (Texture2D.height bear.texture)))
    (*Dest rect*)
    (dest_rect_custom bear x y)
    (*Origin vector*)
    (origin_vect_custom bear)
    bear.facing
    (Color.create 255 255 255 255)

(*Draws the placed bears in the game*)
let rec draw_bears (bears : bear list) =
  match bears with
  | [] -> ()
  | bear :: rest ->
      draw_bear bear;
      draw_bears rest

let rec draw_menu_bears (menu_bears : bear list) =
  match menu_bears with
  | [] -> ()
  | bear :: rest ->
      draw_menu_bear bear;
      draw_menu_bears rest

let draw_selected_bear (bear : bear option) =
  match bear with None -> () | Some bear -> draw_bear bear

let check_circle_collision circ_one circ_two radius =
  Vector2.distance circ_one circ_two < 2. *. radius

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

let rec remove_bears bear_lst =
  match bear_lst with
  | [] -> []
  | bear :: rest ->
      if bear.sold then remove_bears rest else bear :: remove_bears rest
