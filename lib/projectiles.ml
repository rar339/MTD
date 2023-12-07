open Raylib
open Constants
open Bears
open Balloons

let bullet_radius = 7.5

(*-Pierce is how many balloons a bullet can pierce through.
  -Damage is how many layers of a bullet will go through.
  -Hits is the list of balloons that a bullet has already collided with, used so
  a bullet cannot hit the same balloon twice.
  -Origin is the position the bullet was fired from, so that we can delete bullets
  once they leave the bear's range.*)
type bullet = {
  origin : bear;
  mutable position : Vector2.t;
  velocity : Vector2.t;
  color : Color.t;
  image : Texture2D.t option;
  radius : float;
  mutable pierce : int;
  mutable hits : Balloons.balloon list;
  mutable timer : int;
  target : balloon option;
}

let bullet_collection : bullet list ref = ref []

let vector_angle (velocity : Vector2.t) =
  Vector2.angle velocity (Vector2.create 1. 0.)

(******************************************************************************)
let calculate_new_vel (bear : Bears.bear) (balloon_pos : Vector2.t) =
  let angle = Vector2.angle bear.position balloon_pos in
  Vector2.create
    (cos angle *. bear.projectile_speed)
    (sin angle *. bear.projectile_speed)

let projectile_moving_calc (bear : Bears.bear) (balloon : Balloons.balloon) =
  let enemy_vel_y = Vector2.y balloon.velocity in
  let enemy_vel_x = Vector2.x balloon.velocity in
  let bullet_speed = bear.projectile_speed in
  let x_target = Vector2.x balloon.position in
  let y_target = Vector2.y balloon.position in
  let x_origin = Vector2.x bear.position in
  let y_origin = Vector2.y bear.position in
  let first_term = (enemy_vel_y ** 2.0) *. ((x_target -. x_origin) ** 2.0) in
  let second_term =
    2.0 *. enemy_vel_y *. enemy_vel_x *. (x_target -. x_origin)
    *. (y_target -. y_origin)
  in
  let third_term = (enemy_vel_x ** 2.0) *. ((y_target -. y_origin) ** 2.0) in
  let fourth_term = (bullet_speed ** 2.0) *. ((x_target -. x_origin) ** 2.0) in
  let fifth_term = (bullet_speed ** 2.0) *. ((y_target -. y_origin) ** 2.0) in
  let sixth_term = enemy_vel_y *. (y_target -. y_origin) in
  let seventh_term = enemy_vel_x *. (x_target -. x_origin) in
  let radical =
    sqrt
      (~-.first_term +. second_term -. third_term +. fourth_term +. fifth_term)
  in
  let n_num = ~-.radical -. sixth_term -. seventh_term in
  let n_dem =
    (enemy_vel_y ** 2.0) +. (enemy_vel_x ** 2.0) -. (bullet_speed ** 2.0)
  in
  let n = n_num /. n_dem in

  let new_x_t = x_target +. (enemy_vel_x *. n) in
  let new_y_t = y_target +. (enemy_vel_y *. n) in

  (n, calculate_new_vel bear (Vector2.create new_x_t new_y_t))

(******************************************************************************)

let determine_projectile_img (bear : Bears.bear) =
  match bear.bear_type with
  | Dragon -> !fireball_img
  | Dart -> !dartshot_img
  | _ -> !hockeypuck_img

let is_balloon_in_range (bear : Bears.bear) (balloon : Balloons.balloon) : bool
    =
  Vector2.distance bear.position balloon.position <= bear.range

(**Determine which balloon is closer to the next turn point.*)
let compare_balloons_helper (balloon1 : Balloons.balloon)
    (balloon2 : Balloons.balloon) : int =
  let x1, y1, _ = List.nth !Balloonpath.turn_points balloon1.current_turn in
  let turn_pos = Vector2.create (float_of_int x1) (float_of_int y1) in
  let dist1 = Vector2.distance balloon1.position turn_pos in
  let dist2 = Vector2.distance balloon2.position turn_pos in
  if dist1 < dist2 then -1 else 1

(**Determine which balloon is in front of the other.*)
let compare_balloons (balloon1 : Balloons.balloon) (balloon2 : Balloons.balloon)
    : int =
  if balloon1.current_turn > balloon2.current_turn then -1
  else if balloon1.current_turn < balloon2.current_turn then 1
  else compare_balloons_helper balloon1 balloon2

(**Sort the balloon list to be from closest to the end to furthest from the end.*)
let sort_balloons balloon_lst = List.sort compare_balloons balloon_lst

let rec find_target (bear : Bears.bear) (balloons : Balloons.balloon list) :
    Balloons.balloon option =
  match balloons with
  | [] -> None
  | first :: rest ->
      if is_balloon_in_range bear first then Some first
      else find_target bear rest

(*Fires a dart.*)
let fire_dart (bear : Bears.bear) (balloon : Balloons.balloon) =
  let _, velocity = projectile_moving_calc bear balloon in
  let new_color =
    match bear.bear_type with Dragon -> Color.red | _ -> Color.black
  in
  bullet_collection :=
    {
      origin = bear;
      position = bear.position;
      velocity;
      color = new_color;
      image = determine_projectile_img bear;
      radius = bullet_radius;
      pierce = 1;
      hits = [];
      timer = 1;
      target = None;
    }
    :: !bullet_collection

(* Creates dart that will shoot on eight sides of bear *)
let init_puck (bear : bear) (v1 : float) (v2 : float) =
  let velocity = Vector2.create v1 v2 in
  {
    origin = bear;
    position = bear.position;
    velocity;
    color = Color.black;
    image = determine_projectile_img bear;
    radius = bullet_radius;
    pierce = 1;
    hits = [];
    timer = 1;
    target = None;
  }

(*Fires a dart in a nail shooter way.*)
let fire_pucks (bear : Bears.bear) =
  bullet_collection :=
    init_puck bear (-7.5) 7.5 :: init_puck bear (-7.5) 0.0
    :: init_puck bear (-7.5) (-7.5)
    :: init_puck bear 7.5 7.5 :: init_puck bear 7.5 0.0
    :: init_puck bear 7.5 (-7.5) :: init_puck bear 0.0 7.5
    :: init_puck bear 0.0 (-7.5) :: !bullet_collection

let fire_sniper (bear : Bears.bear) (balloon : Balloons.balloon) =
  let time, velocity = projectile_moving_calc bear balloon in
  bullet_collection :=
    {
      origin = bear;
      position = bear.position;
      velocity;
      color = Color.black;
      image = determine_projectile_img bear;
      radius = bullet_radius;
      pierce = 1;
      hits = [];
      timer = round_float time;
      target = Some balloon;
    }
    :: !bullet_collection

(**Returns the list of balloons in range of the bear.*)
let rec find_balloons_in_range bear balloon_list =
  match balloon_list with
  | [] -> []
  | balloon :: t ->
      if is_balloon_in_range bear balloon then
        balloon :: find_balloons_in_range bear t
      else find_balloons_in_range bear t

(**Applies a bear's freeze to all the balloons in the given list.*)
let rec freeze_balloons (bear : Bears.bear) balloon_list =
  match balloon_list with
  | [] -> ()
  | (balloon : Balloons.balloon) :: t ->
      balloon.freeze_duration <- bear.freeze_duration;
      freeze_balloons bear t

(**Fires the given polar bear.*)
let fire_polar (bear : Bears.bear) balloon_list =
  begin_drawing ();
  draw_circle
    (Constants.round_float (Vector2.x bear.position))
    (Constants.round_float (Vector2.y bear.position))
    bear.range
    (Color.create 138 222 235 100);
  end_drawing ();
  let balloons_in_range = find_balloons_in_range bear balloon_list in
  freeze_balloons bear balloons_in_range

let init_projectile (bear : Bears.bear) (balloon : Balloons.balloon)
    balloon_list =
  match bear with
  | { bear_type = Dart; _ } -> fire_dart bear balloon
  | { bear_type = Hockey; _ } -> fire_pucks bear
  | { bear_type = Polar; _ } -> fire_polar bear balloon_list
  | { bear_type = Dragon; _ } -> fire_dart bear balloon
  | { bear_type = Sniper; _ } -> fire_sniper bear balloon

(**Precondition: balloons must be in the order they appear on the screen.*)
let rec fire_all_shots (bears : Bears.bear list)
    (balloons : Balloons.balloon list) =
  match bears with
  | [] -> ()
  | first :: rest -> (
      match find_target first balloons with
      | None ->
          first.counter <- first.counter - 1;
          fire_all_shots rest balloons
      | Some balloon ->
          if first.counter <= 0 then (
            first.counter <- first.attack_speed;
            init_projectile first balloon balloons)
          else first.counter <- first.counter - 1;
          fire_all_shots rest balloons)

let update_bullet bullet =
  bullet.position <- Vector2.add bullet.position bullet.velocity

let rec update_bullets bullets =
  match bullets with
  | [] -> ()
  | first :: rest ->
      update_bullet first;
      update_bullets rest

let rec dart_collisions (bear : bear) bullet balloon_list =
  match balloon_list with
  | [] -> ()
  | balloon :: t ->
      if
        (not (List.mem balloon bullet.hits))
        && check_collision_circle_rec bullet.position bullet_radius
             (get_hitbox balloon)
      then (
        (*What to do when a collision occurs.*)
        bullet.pierce <- bullet.pierce - 1;
        bullet.hits <- balloon :: bullet.hits;
        Balloons.hit_update bear balloon)
      else dart_collisions bear bullet t

(*Updates bullets and balloons if a collision has occurred. Compares
   given bullet with each balloon in balloon_list.*)
let update_bullet_collision bullet balloon_list =
  match bullet.origin with
  | { bear_type = Dart; _ } as bear -> dart_collisions bear bullet balloon_list
  | { bear_type = Hockey; _ } as bear ->
      dart_collisions bear bullet balloon_list
  | { bear_type = Polar; _ } as bear -> dart_collisions bear bullet balloon_list
  | { bear_type = Dragon; _ } as bear ->
      dart_collisions bear bullet balloon_list
  | { bear_type = Sniper; _ } -> bullet.timer <- bullet.timer - 1

let rec update_collisions bullet_list balloon_list =
  match bullet_list with
  | [] -> ()
  | bullet :: t ->
      update_bullet_collision bullet balloon_list;
      update_collisions t balloon_list

let check_screen_bounds bullet =
  let x = Vector2.x bullet.position in
  let y = Vector2.y bullet.position in
  x > !screen_width +. 10. || x < -10.0 || y > !screen_width +. 10. || y < -10.0

(*Check if a bullet is within it's tower's range.*)
let check_tower_bounds bullet =
  Vector2.distance bullet.origin.position bullet.position
  > bullet.origin.range *. 1.5

(*Delete bullets that have left the bounds of the screen or their tower's
   range. TRUE if it is out of bounds and should be deleted.*)
let check_bullet_bounds bullet =
  check_screen_bounds bullet || check_tower_bounds bullet

let time_expired bullet =
  match bullet.origin.bear_type with
  | Sniper ->
      if bullet.timer <= 0 then (
        update_balloon_status bullet.origin (Option.get bullet.target);
        true)
      else false
  | _ -> false

(*Remove bullets whether they are out of bounds or have collided.*)
let rec remove_bullets bullet_list =
  match bullet_list with
  | [] -> []
  | bullet :: t ->
      if check_bullet_bounds bullet || bullet.pierce = 0 || time_expired bullet
      then remove_bullets t
      else bullet :: remove_bullets t

let draw_bullet bullet =
  match bullet.origin.bear_type with
  | Dragon ->
      draw_texture_ex (Option.get bullet.image) bullet.position
        (180. /. Float.pi *. vector_angle bullet.velocity)
        1.
        (Color.create 255 255 255 255)
  | _ ->
      draw_texture_ex (Option.get bullet.image) bullet.position
        (180. /. Float.pi *. vector_angle bullet.velocity)
        1.
        (Color.create 255 255 255 255)

let rec draw_bullets bullets =
  match bullets with
  | [] -> ()
  | first :: rest ->
      draw_bullet first;
      draw_bullets rest

let rec update_angle_bear (bear : Bears.bear) (balloons : Balloons.balloon list)
    =
  match balloons with
  | [] -> ()
  | first :: rest ->
      if
        is_balloon_in_range bear first
        && bear.bear_type <> Hockey && bear.bear_type <> Polar
      then
        bear.facing <-
          (180. /. Float.pi *. Vector2.angle first.position bear.position)
          -. 90.
      else update_angle_bear bear rest

let rec update_angles (bears : Bears.bear list)
    (balloons : Balloons.balloon list) =
  match bears with
  | [] -> ()
  | first :: rest ->
      update_angle_bear first balloons;
      update_angles rest balloons
