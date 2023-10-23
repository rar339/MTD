let width = 1100
let height = 720
let enemy_vel_x = 2.0
let enemy_vel_y = -2.0
let bullet_speed = 5.0
let round_float x = int_of_float (Float.round x)
let count = ref 0

let distance x1 y1 x2 y2 =
  let diff_x = x2 -. x1 in
  let diff_y = y2 -. y1 in
  let square_x = diff_x ** 2.0 in
  let square_y = diff_y ** 2.0 in
  sqrt (square_x +. square_y)

module Bullet = struct
  type t = {
    x : float;
    y : float;
    x_rate : float;
    y_rate : float;
    radius : float;
    color : Raylib.Color.t;
  }

  let init_bullet x y x_rate y_rate radius color =
    { x; y; x_rate; y_rate; radius; color }

  let draw_bullet bullet =
    Raylib.draw_circle (round_float bullet.x) (round_float bullet.y)
      bullet.radius Raylib.Color.green

  let get_x bullet = bullet.x
  let get_y bullet = bullet.y
  let get_xr bullet = bullet.x_rate
  let get_yr bullet = bullet.y_rate
  let get_color bullet = bullet.color
end

module BulletCollection = struct
  type t = Bullet.t ref list

  let empty = []
  let add_bullet new_bullet collection = ref new_bullet :: collection

  let rec draw_bullets collection =
    match collection with
    | [] -> ()
    | h :: t ->
        Bullet.draw_bullet !h;
        draw_bullets t
end

let bullet_collection = ref BulletCollection.empty

let calculate_angle float_x_o float_y_o float_x_t float_y_t =
  let open Raylib in
  let player_vector = Vector2.create float_x_o float_y_o in
  let enemy_vector = Vector2.create float_x_t float_y_t in

  let angle = Vector2.angle player_vector enemy_vector in
  (cos angle *. bullet_speed, sin angle *. bullet_speed)

let niko_projectile_moving_calc x_origin y_origin x_target y_target =
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

  calculate_angle x_origin y_origin new_x_t new_y_t

module Circle = struct
  type t = {
    radius : float;
    x : float;
    y : float;
    color : Raylib.Color.t;
    range : float;
  }

  let init_circle r x y color = { radius = r; x; y; color; range = r +. 20. }

  let draw_circle circle =
    Raylib.draw_circle (round_float circle.x) (round_float circle.y)
      circle.radius circle.color

  let update_color circle color = { circle with color }
  let update_pos circle x y = { circle with x; y }
  let get_x circle = circle.x
  let get_y circle = circle.y

  let fire1 circle enemy =
    let x_rate, y_rate =
      niko_projectile_moving_calc circle.x circle.y enemy.x enemy.y
    in
    let new_bullet =
      Bullet.init_bullet circle.x circle.y x_rate y_rate 10.0 Raylib.Color.green
    in
    bullet_collection :=
      BulletCollection.add_bullet new_bullet !bullet_collection

  let rec fire_all circle enemy_collection =
    match enemy_collection with
    | [] -> ()
    | h :: t ->
        fire1 circle !h;
        fire_all circle t
end

module CircleCollection = struct
  type t = Circle.t list

  let empty = []
  let add_circle new_circle collection = new_circle :: collection

  let draw_circles collection =
    let rec loop inp count =
      match inp with
      | [] -> ()
      | h :: t ->
          Circle.draw_circle h;
          loop t (count - 1)
    in
    loop collection (List.length collection)
end

module EnemyCollection = struct
  type t = Circle.t ref list

  let empty = []
  let add_circle new_circle collection = ref new_circle :: collection

  let draw_enemies collection =
    let rec loop inp count =
      match inp with
      | [] -> ()
      | h :: t ->
          Circle.draw_circle !h;
          loop t (count - 1)
    in
    loop collection (List.length collection)
end

let base_circle =
  Circle.init_circle 50.
    (float_of_int (width / 2))
    (float_of_int (height / 2))
    Raylib.Color.red

let select = ref false

let selected_circle =
  ref
    (Circle.init_circle 50.
       (float_of_int (width / 2))
       (float_of_int (height / 2))
       Raylib.Color.red)

let circle_collection = ref CircleCollection.empty
let enemy_collection = ref EnemyCollection.empty

let setup () =
  enemy_collection :=
    EnemyCollection.add_circle
      (Circle.init_circle 1.
         (float_of_int (width - 200))
         (float_of_int (height - 700))
         Raylib.Color.gold)
      !enemy_collection;
  ()

let enemy_movement enemy =
  let deref = !enemy in
  (*Handle wrapping*)
  let new_x =
    if Circle.get_x deref +. enemy_vel_x > float_of_int width then 0.0
    else if Circle.get_x deref +. enemy_vel_x < 0.0 then float_of_int width
    else Circle.get_x deref +. enemy_vel_x
  in
  let new_y =
    if Circle.get_y deref +. enemy_vel_y > float_of_int height then 0.0
    else if Circle.get_y deref +. enemy_vel_y < 0.0 then float_of_int height
    else Circle.get_y deref +. enemy_vel_y
  in
  let updated_enemy = Circle.init_circle 20. new_x new_y Raylib.Color.gold in
  enemy := updated_enemy

let enemy_update () =
  let rec loop (collection : Circle.t ref list) =
    match collection with
    | [] -> ()
    | h :: t ->
        enemy_movement h;
        loop t
  in
  loop !enemy_collection

let bullet_movement bullet =
  let deref = !bullet in
  let new_x = Bullet.get_x deref +. Bullet.get_xr deref in
  let new_y = Bullet.get_y deref +. Bullet.get_yr deref in
  let updated_bullet =
    Bullet.init_bullet new_x new_y (Bullet.get_xr deref) (Bullet.get_yr deref)
      10.0 (Bullet.get_color deref)
  in
  bullet := updated_bullet

let bullet_check bullet =
  let x = int_of_float (Bullet.get_x bullet) in
  let y = int_of_float (Bullet.get_y bullet) in
  if x > width || x < 0 || y > height || y < 0 then true else false

let rec bullet_bounds collection =
  match collection with
  | [] -> []
  | h :: t -> if bullet_check !h then bullet_bounds t else h :: bullet_bounds t

let bullet_update () =
  let rec loop (collection : Bullet.t ref list) =
    match collection with
    | [] -> ()
    | h :: t ->
        bullet_movement h;
        loop t
  in
  loop !bullet_collection;
  bullet_collection := bullet_bounds !bullet_collection

let update_game () =
  let open Raylib in
  if is_mouse_button_released Left then (
    select := false;
    circle_collection :=
      CircleCollection.add_circle !selected_circle !circle_collection)
  else if !select then
    selected_circle :=
      Circle.init_circle base_circle.radius
        (float_of_int (get_mouse_x ()))
        (float_of_int (get_mouse_y ()))
        Color.green
  else if
    is_mouse_button_down Left
    && distance (Circle.get_x base_circle) (Circle.get_y base_circle)
         (float_of_int (get_mouse_x ()))
         (float_of_int (get_mouse_y ()))
       <= base_circle.radius
  then (
    select := true;
    selected_circle := base_circle)
  else ();
  if Raylib.is_key_pressed Space then
    Circle.fire_all base_circle !enemy_collection;
  enemy_update ();
  bullet_update ()

let draw_game () =
  let open Raylib in
  begin_drawing ();
  Circle.draw_circle base_circle;
  if !select then Circle.draw_circle !selected_circle;
  CircleCollection.draw_circles !circle_collection;
  EnemyCollection.draw_enemies !enemy_collection;
  BulletCollection.draw_bullets !bullet_collection;
  draw_fps 10 10;
  clear_background Color.lightgray;
  end_drawing ()

(*Main game loop*)
let loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else (
    if !count = 0 then (
      count := 1;
      setup ();
      update_game ();
      draw_game ())
    else update_game ();
    draw_game ())
