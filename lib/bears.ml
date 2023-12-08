open Raylib
open Constants
open Yojson.Basic.Util

type bear_types = Dart | Hockey | Polar | Sniper | Dragon

(*Width and height are temporary, shouldn't be needed if all images are the same
   size.*)
type bear = {
  bear_type : bear_types;
  mutable range : float;
  mutable cost : int;
  mutable upgrades : int;
  mutable position : Raylib.Vector2.t;
  texture : Raylib.Texture2D.t;
  image_width : float;
  image_height : float;
  is_placed : bool;
  mutable attack_speed : int;
  mutable counter : int;
  mutable projectile_speed : float;
  mutable sold : bool;
  mutable damage : int;
  mutable facing : float;
  mutable pops_lead : bool;
  mutable freeze_duration : int;
}

let bear_collection : bear list ref = ref []
let bear_radius = 40.
let menu_bear_radius = 52.
let get_x bear = Vector2.x bear.position
let get_y bear = Vector2.y bear.position
let fire_rect_length = 3. *. !screen_height /. 28.
let fire_rect_width = bear_radius *. 2.
let selected_bear : bear option ref = ref None

(*The bears displayed on the menu.*)
let menu_bears : bear list ref = ref []

let determine_image menu_bear bear_type =
  match bear_type with
  | Dart ->
      if menu_bear then Option.get !menu_dartbear_img
      else Option.get !dartbear_img
  | Hockey ->
      if menu_bear then Option.get !menu_hockeybear_img
      else Option.get !hockeybear_img
  | Polar ->
      if menu_bear then Option.get !menu_polarbear_img
      else Option.get !polarbear_img
  | Sniper ->
      if menu_bear then Option.get !menu_sniperbear_img
      else Option.get !sniperbear_img
  | Dragon ->
      if menu_bear then Option.get !menu_dragonbear_img
      else Option.get !dragonbear_img

let string_of_beartype bear_type =
  match bear_type with
  | Dart -> "Dart"
  | Hockey -> "Hockey"
  | Polar -> "Polar"
  | Sniper -> "Sniper"
  | Dragon -> "Dragon"

(**Extract the json dictionary containing the properties for the given bear type.*)
let extract_bear_properties (bear_type : bear_types) =
  let json = Yojson.Basic.from_file "./data/bears.json" in
  json |> member (string_of_beartype bear_type)

(**Creates a bear given whether the bear is a menu bear the type of a bear, and its position.*)
let make_bear (menu_bear : bool) (bear_type : bear_types) pos =
  let image = determine_image menu_bear bear_type in
  let image_width = float_of_int (Texture.width image) in
  let image_height = float_of_int (Texture.height image) in
  let properties = extract_bear_properties bear_type in
  {
    bear_type;
    range = properties |> member "range" |> to_float;
    cost = properties |> member "cost" |> to_int;
    upgrades = 0;
    position = pos;
    texture = image;
    image_width;
    image_height;
    is_placed = true;
    attack_speed =
      (properties |> member "attack_speed" |> to_int) / !Constants.speed_mult;
    counter = 0;
    projectile_speed =
      (properties |> member "projectile_speed" |> to_float)
      *. float_of_int !Constants.speed_mult;
    sold = false;
    damage = properties |> member "damage" |> to_int;
    pops_lead = bear_type = Dragon || bear_type = Sniper;
    facing = 0.;
    freeze_duration = 30;
  }

let generate_menu_bears screen_width screen_height =
  [
    make_bear true Dart
      (Vector2.create (5.55 *. screen_width /. 7.) (0.8 *. screen_height /. 4.));
    make_bear true Hockey
      (Vector2.create (6. *. screen_width /. 7.) (0.8 *. screen_height /. 4.));
    make_bear true Polar
      (Vector2.create (6.45 *. screen_width /. 7.) (0.8 *. screen_height /. 4.));
    make_bear true Sniper
      (Vector2.create (5.75 *. screen_width /. 7.) (1.2 *. screen_height /. 4.));
    make_bear true Dragon
      (Vector2.create (6.25 *. screen_width /. 7.) (1.2 *. screen_height /. 4.));
  ]

(*Returns the bear clicked, if any.*)
let rec determine_bear_clicked click_pos bear_list =
  match bear_list with
  | [] -> None
  | bear :: rest ->
      if
        check_collision_point_circle click_pos bear.position bear_radius
        && !cash >= bear.cost
      then Some bear
      else determine_bear_clicked click_pos rest

let rec determine_bear_hovered click_pos bear_list =
  match bear_list with
  | [] -> None
  | bear :: rest ->
      if
        check_collision_point_circle click_pos bear.position bear_radius
      then Some bear
      else determine_bear_hovered click_pos rest

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
  | Sniper -> Rectangle.create x y (bear_radius *. 3.5) (bear_radius *. 3.5)
  | Polar -> Rectangle.create x y (bear_radius *. 3.2) (bear_radius *. 3.2)
  | Dragon -> Rectangle.create x y (bear_radius *. 4.) (bear_radius *. 4.)

(* Helps with placement of images to ensure its centered, the factor must be
   1/2 of factor for dest_rect*)
let origin_vect_custom (bear : bear) =
  match bear.bear_type with
  | Dart -> Vector2.create (bear_radius *. 1.5) (bear_radius *. 1.5)
  | Hockey -> Vector2.create (bear_radius *. 1.6) (bear_radius *. 1.6)
  | Sniper -> Vector2.create (bear_radius *. 1.6) (bear_radius *. 1.6)
  | Polar -> Vector2.create (bear_radius *. 1.6) (bear_radius *. 1.6)
  | Dragon -> Vector2.create (bear_radius *. 2.) (bear_radius *. 2.)

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

let rec update_bear_firing_rate = function
  | bear :: t ->
      bear.attack_speed <-
        (if !Constants.speed_mult = 2 then bear.attack_speed / 2
         else bear.attack_speed * 2);
      bear.projectile_speed <-
        (if !Constants.speed_mult = 2 then bear.projectile_speed *. 2.
         else bear.projectile_speed /. 2.);
      update_bear_firing_rate t
  | [] -> ()
