open Raylib
open Constants
open Bears
open Balloons

let bullet_radius = 10.

(*-Pierce is how many balloons a bullet can pierce through.
  -Damage is how many layers of a bullet will go through.
  -Hits is the list of balloons that a bullet has already collided with, used so
  a bullet cannot hit the same balloon twice.*)
type bullet = {
  projectile : bear_types;
  mutable position : Vector2.t;
  velocity : Vector2.t;
  color : Color.t;
  image : Texture2D.t option;
  radius : float;
  mutable pierce : int;
  damage : int;
  mutable hits : Balloons.balloon list;
}

let bullet_collection : bullet list ref = ref []

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

  calculate_new_vel bear (Vector2.create new_x_t new_y_t)

(******************************************************************************)

let is_balloon_in_range (bear : Bears.bear) (balloon : Balloons.balloon) : bool
    =
  Vector2.distance bear.position balloon.position <= bear.range

(*Precondition: balloons must be in the order they appear on the screen.*)
let rec find_target (bear : Bears.bear) (balloons : Balloons.balloon list) :
    Balloons.balloon option =
  match balloons with
  | [] -> None
  | first :: rest ->
      if is_balloon_in_range bear first then Some first
      else find_target bear rest

(*Fires a dart.*)
let fire_dart (bear : Bears.bear) (balloon : Balloons.balloon) =
  let velocity = projectile_moving_calc bear balloon in
  bullet_collection :=
    {
      projectile = Dart;
      position = bear.position;
      velocity;
      color = Color.black;
      image = None;
      radius = bullet_radius;
      pierce = 1;
      damage = 1;
      hits = [];
    }
    :: !bullet_collection

let init_projectile (bear : Bears.bear) (balloon : Balloons.balloon) =
  match bear with
  | { bear_type = Dart; _ } -> fire_dart bear balloon
  | { bear_type = Hockey; _ } -> ()
  | { bear_type = Pumpkin; _ } -> ()
  | { bear_type = Dragon; _ } -> ()
  | { bear_type = Ezra; _ } -> ()

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
            init_projectile first balloon)
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

let rec dart_collisions bullet balloon_list =
  match balloon_list with
  | [] -> ()
  | balloon :: t ->
      if
        (not (List.mem balloon bullet.hits))
        && check_collision_circle_rec bullet.position bullet_radius
             (get_hitbox balloon)
      then (
        bullet.pierce <- bullet.pierce - 1;
        bullet.hits <- balloon :: bullet.hits;
        Balloons.hit_update bullet.damage balloon)
      else dart_collisions bullet t

(*Updates bullets and balloons if a collision has occurred. Compares
   given bullet with each balloon in balloon_list.*)
let update_bullet_collision bullet balloon_list =
  match bullet with
  | { projectile = Dart; _ } -> dart_collisions bullet balloon_list
  | { projectile = Hockey; _ } -> ()
  | { projectile = Pumpkin; _ } -> ()
  | { projectile = Dragon; _ } -> ()
  | { projectile = Ezra; _ } -> ()

let rec update_collisions bullet_list balloon_list =
  match bullet_list with
  | [] -> ()
  | bullet :: t ->
      update_bullet_collision bullet balloon_list;
      update_collisions t balloon_list

(*Delete bullets that have gone off the screen.*)
let check_bullet_bounds bullet =
  let x = Vector2.x bullet.position in
  let y = Vector2.y bullet.position in
  x > !screen_width +. 10. || x < -10.0 || y > !screen_width +. 10. || y < -10.0

(*Remove bullets whether they are out of bounds or have collided.*)
let rec remove_bullets bullet_list =
  match bullet_list with
  | [] -> []
  | bullet :: t ->
      if check_bullet_bounds bullet || bullet.pierce = 0 then remove_bullets t
      else bullet :: remove_bullets t

let draw_bullet bullet =
  draw_circle
    (round_float (Vector2.x bullet.position))
    (round_float (Vector2.y bullet.position))
    bullet.radius bullet.color

let rec draw_bullets bullets =
  match bullets with
  | [] -> ()
  | first :: rest ->
      draw_bullet first;
      draw_bullets rest
