open Raylib

(*Constants********************************************************************)
module Constants = struct
  (*Screen Constants*)
  let screen_width = 900
  let screen_height = 900

  (*Bullet Constants*)
  let bullet_speed = 10.0
  let bullet_radius = 5.0
  let bullet_color = Color.black

  (*Player Constants*)
  let player_radius = 25.0
  let player_color = Raylib.Color.blue

  (*Enemy Constants*)
  let enemy_color = Color.red
  let enemy_radius = 30.
  let enemy_vel_x = 5.
  let enemy_vel_y = 0.
  let og_circle_x = float_of_int (2 * screen_width / 3)
  let og_circle_y = float_of_int (screen_height / 2)
end

open Constants

(******************************************************************************)
module Player = struct
  type t = { x : float; y : float; created : bool; placed : bool }

  let init_player =
    {
      x = float_of_int (screen_width / 3);
      y = float_of_int (screen_height / 2);
      created = false;
      placed = false;
    }

  let move_player (selected : Vector2.t) : t =
    {
      x = Vector2.x selected;
      y = Vector2.y selected;
      created = true;
      placed = false;
    }

  let placed_player (player : t) = { player with placed = true }

  let draw_player (player : t) =
    if player.created && player.placed = false then
      draw_circle (int_of_float player.x) (int_of_float player.y) player_radius
        Color.green
    else if player.placed = true then
      draw_circle (int_of_float player.x) (int_of_float player.y) player_radius
        player_color
end

open Player

(******************************************************************************)
module Enemy = struct
  let enemy_path_radius = 200.

  type t = {
    x : float;
    y : float;
    pos_angle : float;
    x_vel : float;
    y_vel : float;
    hit_points : int;
    color : Color.t;
  }

  let init_enemy =
    {
      x = float_of_int (screen_width / 2);
      y = float_of_int (screen_height / 2);
      pos_angle = 90.;
      x_vel = enemy_vel_x;
      y_vel = enemy_vel_y;
      hit_points = 50;
      color = Color.red;
    }

  (* MOVE ENEMY IN A CIRCLE *)
  (* let move_enemy (enemy : t) =
     let new_angle = enemy.pos_angle +. 2.5 in
     {
       x =
         float_of_int (screen_width / 2)
         +. (enemy_path_radius *. cos (radians new_angle));
       y =
         float_of_int (screen_width / 2)
         +. (enemy_path_radius *. sin (radians new_angle));
       pos_angle = new_angle;
       hit_points = enemy.hit_points;
       color = enemy.color;
       velocity = 0.;
     } *)

  (* let move_enemy_x (enemy : t) : t =
       if enemy.x_vel > 0. && enemy.x +. enemy_radius > float_of_int screen_width
       then { enemy with x_vel = enemy.x_vel *. -1. }
       else if enemy.x_vel < 0. && enemy.x -. enemy_radius < 0. then
         { enemy with x_vel = enemy.x_vel *. -1. }
       else { enemy with x = enemy.x +. enemy.x_vel }

     let move_enemy_y (enemy : t) : t =
       if enemy.y_vel > 0. && enemy.y +. enemy_radius > float_of_int screen_width
       then { enemy with y_vel = enemy.y_vel *. -1. }
       else if enemy.y_vel < 0. && enemy.y -. enemy_radius < 0. then
         { enemy with y_vel = enemy.y_vel *. -1. }
       else { enemy with y = enemy.y +. enemy.y_vel } *)

  let move_enemy_x (enemy : t) : t =
    if enemy.x -. enemy_radius > float_of_int screen_width then
      { enemy with x = 0. }
    else { enemy with x = enemy.x +. enemy_vel_x }

  let move_enemy_y (enemy : t) : t =
    if enemy.y -. enemy_radius > float_of_int screen_height then
      { enemy with y = 0. }
    else { enemy with y = enemy.y +. enemy_vel_y }

  let move_enemy (enemy : t) =
    let temp_enemy = move_enemy_x enemy in
    move_enemy_y temp_enemy

  let draw_enemy (enemy : t) =
    draw_circle (int_of_float enemy.x) (int_of_float enemy.y) enemy_radius
      enemy_color
end

open Enemy

(******************************************************************************)
module Bullet = struct
  type t = {
    x : float;
    y : float;
    x_rate : float;
    y_rate : float;
    color : Raylib.Color.t;
  }

  let calculate_angle (player : Player.t) (enemy : Enemy.t) =
    let player_vector = Vector2.create player.x player.y in
    let enemy_vector = Vector2.create enemy.x enemy.y in

    Vector2.angle player_vector enemy_vector

  let init_bullet (player : Player.t) (change_x : float) (change_y : float) : t
      =
    {
      x = player.x;
      y = player.y;
      x_rate = change_x;
      y_rate = change_y;
      color = bullet_color;
    }

  let move_bullet shot : t =
    { shot with x = shot.x +. shot.x_rate; y = shot.y +. shot.y_rate }

  let check_bullet (bullet : t) (enemy : Enemy.t) =
    if
      bullet.x > float_of_int screen_width
      || bullet.x < 0.
      || bullet.y > float_of_int screen_height
      || bullet.y < 0.
      || bullet.x < enemy.x +. enemy_radius
         && bullet.x > enemy.x -. enemy_radius
         && bullet.y < enemy.y +. enemy_radius
         && bullet.y > enemy.y -. enemy_radius
    then false
    else true

  let draw_bullet (shot : t) =
    Raylib.draw_circle (int_of_float shot.x) (int_of_float shot.y) bullet_radius
      shot.color
end

open Bullet

(******************************************************************************)
module PlayerCollection = struct
  type t = Player.t list

  let add_players (player : Player.t) (collection : t) : t =
    player :: collection

  let rec draw_players (collection : t) =
    match collection with
    | [] -> ()
    | h :: t ->
        draw_player h;
        draw_players t
end

open PlayerCollection

(******************************************************************************)
module BulletCollection = struct
  type t = Bullet.t list

  let add_bullet (bullet : Bullet.t) (bullets : Bullet.t list) =
    bullet :: bullets

  let rec move_bullets (bullets : t) (enemy : Enemy.t) =
    match bullets with
    | [] -> []
    | bullet :: rest ->
        if check_bullet bullet enemy then
          move_bullet bullet :: move_bullets rest enemy
        else move_bullets rest enemy

  let rec draw_bullets (bullets : t) =
    match bullets with
    | [] -> ()
    | bullet :: rest ->
        draw_bullet bullet;
        draw_bullets rest
end

open BulletCollection

(******************************************************************************)
module GameLogic = struct
  let calculate_angle float_x_o float_y_o float_x_t float_y_t =
    let open Raylib in
    let player_vector = Vector2.create float_x_o float_y_o in
    let enemy_vector = Vector2.create float_x_t float_y_t in

    Vector2.angle player_vector enemy_vector

  let temp float_x_o float_y_o float_x_t float_y_t =
    let angle = calculate_angle float_x_o float_y_o float_x_t float_y_t in
    (cos angle *. bullet_speed, sin angle *. bullet_speed)

  let projectile_calc (enemy : Enemy.t) x_origin y_origin x_target y_target =
    let first_term = (enemy.y_vel ** 2.0) *. ((x_target -. x_origin) ** 2.0) in
    let second_term =
      2.0 *. enemy.y_vel *. enemy.x_vel *. (x_target -. x_origin)
      *. (y_target -. y_origin)
    in
    let third_term = (enemy.x_vel ** 2.0) *. ((y_target -. y_origin) ** 2.0) in
    let fourth_term =
      (bullet_speed ** 2.0) *. ((x_target -. x_origin) ** 2.0)
    in
    let fifth_term = (bullet_speed ** 2.0) *. ((y_target -. y_origin) ** 2.0) in
    let sixth_term = enemy.y_vel *. (y_target -. y_origin) in
    let seventh_term = enemy.x_vel *. (x_target -. x_origin) in
    let radical =
      sqrt
        (~-.first_term +. second_term -. third_term +. fourth_term +. fifth_term)
    in
    let n_num = ~-.radical -. sixth_term -. seventh_term in
    let n_dem =
      (enemy.y_vel ** 2.0) +. (enemy.x_vel ** 2.0) -. (bullet_speed ** 2.0)
    in
    let n = n_num /. n_dem in

    let new_x_t = x_target +. (enemy.x_vel *. n) in
    let new_y_t = y_target +. (enemy.y_vel *. n) in

    temp x_origin y_origin new_x_t new_y_t

  let rec make_bullets (enemy : Enemy.t) (players : PlayerCollection.t)
      (bullets : BulletCollection.t) : BulletCollection.t =
    match players with
    | [] -> bullets
    | player :: rest ->
        let x, y = projectile_calc enemy player.x player.y enemy.x enemy.y in
        make_bullets enemy rest (init_bullet player x y :: bullets)
end

open GameLogic

(******************************************************************************)
let setup () =
  Raylib.init_window screen_width screen_height "Tracker Demo";
  Raylib.set_target_fps 60;
  let selected : Vector2.t ref =
    ref
      (Vector2.create
         (float_of_int (screen_width / 2))
         (float_of_int (screen_height / 2)))
  in
  let player : Player.t ref = ref init_player in
  let enemy : Enemy.t ref = ref init_enemy in
  let bullets : Bullet.t list ref = ref [] in
  let players : PlayerCollection.t ref = ref [] in
  let button_press : bool ref = ref false in
  (selected, player, enemy, bullets, players, button_press)

(******************************************************************************)
let rec loop (selected, player, enemy, bullets, players, button_press) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    (*Creating players logic!**************************************************)
    selected := get_mouse_position ();
    if is_key_pressed Key.A then
      print_endline (string_of_int (List.length !players));
    if
      !button_press = false && is_mouse_button_down Left
      && Vector2.x !selected <= og_circle_x +. player_radius
      && Vector2.x !selected >= og_circle_x -. player_radius
      && Vector2.y !selected <= og_circle_y +. player_radius
      && Vector2.y !selected >= og_circle_y -. player_radius
    then button_press := true;
    player := init_player;

    if is_mouse_button_down Left && !button_press then
      player := move_player !selected;

    if !button_press && is_mouse_button_released Left then (
      button_press := false;
      player := move_player !selected;
      player := placed_player !player;
      players := add_players !player !players);

    (*Bullet Logic*************************************************************)
    if is_key_pressed Key.Space then
      bullets := make_bullets !enemy !players !bullets;

    (*Move the bullets*)
    bullets := move_bullets !bullets !enemy;
    enemy := move_enemy !enemy;

    (*Draw the Game************************************************************)
    begin_drawing ();

    clear_background Color.lightgray;

    (*Draw circle to copy from*)
    draw_circle (int_of_float og_circle_x) (int_of_float og_circle_y)
      player_radius player_color;

    (*draw Player circle*)
    draw_player !player;
    draw_players !players;

    (*draw Enemy circle*)
    draw_enemy !enemy;

    (*draw bullets*)
    draw_bullets !bullets;

    end_drawing ();
    loop (selected, player, enemy, bullets, players, button_press)
