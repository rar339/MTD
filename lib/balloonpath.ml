open Raylib
open Constants

(**turn_points are represented by the type (int * int * int) array. This maps 
    to (x,y,turn number).*)
let turn_points : (int * int * int) list ref = ref []

let draw_turnpoint x_pos y_pos = draw_circle x_pos y_pos 1.0 Color.red

let rec draw_turnpoints (turn_points : (int * int * int) list) =
  match turn_points with
  | [] -> ()
  | (x, y, _) :: t ->
      draw_turnpoint x y;
      draw_turnpoints t

(*Checks if the given balloon is colliding with a turn point, meaning it should
   make a turn. *)
let rec check_turn_collide (balloon : Balloons.balloon)
    (turn_pts : (int * int * int) list) =
  match turn_pts with
  | [] -> None
  | (x, y, i) :: t ->
      if
        check_collision_circle_rec
          (Vector2.create (float_of_int x) (float_of_int y))
          1.
          (Balloons.get_hitbox balloon)
        && balloon.current_turn < i
      then (
        balloon.current_turn <- balloon.current_turn + 1;
        Some (x, y, i))
      else check_turn_collide balloon t

let turn_balloon (balloon : Balloons.balloon) rate (x, y, i) =
  let bal_x = Vector2.x balloon.position in
  let bal_y = Vector2.y balloon.position in
  match i with
  | 1 ->
      balloon.position <-
        Vector2.create (float_of_int x -. (!Balloons.hitbox_width /. 2.0)) bal_y;
      Vector2.create 0.0 rate
  | 2 ->
      balloon.position <-
        Vector2.create bal_x (float_of_int y -. (!Balloons.hitbox_height /. 2.0));
      Vector2.create (-.rate) 0.0
  | 3 ->
      balloon.position <-
        Vector2.create (float_of_int x +. (!Balloons.hitbox_width /. 2.0)) bal_y;
      Vector2.create 0.0 (-.rate)
  | 4 ->
      balloon.position <-
        Vector2.create bal_x (float_of_int y -. (!Balloons.hitbox_height /. 2.0));
      Vector2.create rate 0.0
  | 5 ->
      balloon.position <-
        Vector2.create (float_of_int x -. (!Balloons.hitbox_width /. 2.0)) bal_y;
      Vector2.create 0.0 (-.rate)
  | 6 ->
      balloon.position <-
        Vector2.create bal_x (float_of_int y +. (!Balloons.hitbox_height /. 2.0));
      Vector2.create rate 0.0
  | 7 ->
      balloon.position <-
        Vector2.create (float_of_int x +. (!Balloons.hitbox_width /. 2.0)) bal_y;
      Vector2.create 0.0 rate
  | 8 ->
      balloon.position <-
        Vector2.create bal_x (float_of_int y +. (!Balloons.hitbox_height /. 2.0));
      Vector2.create (-.rate) 0.0
  | 9 ->
      balloon.position <-
        Vector2.create (float_of_int x -. (!Balloons.hitbox_width /. 2.0)) bal_y;
      Vector2.create 0.0 rate
  | 10 ->
      balloon.position <-
        Vector2.create bal_x (float_of_int y -. (!Balloons.hitbox_height /. 2.0));
      Vector2.create rate 0.0
  | 11 ->
      balloon.position <-
        Vector2.create (float_of_int x -. (!Balloons.hitbox_width /. 2.0)) bal_y;
      Vector2.create 0.0 (-.rate)
  | _ -> failwith "impossible"

(**Moves the balloon, taking into consideration if a turn is reached. If a turn
   is reached, changes the velocity and then moves the balloon.*)
let rec move_balloon (balloon : Balloons.balloon) turn_pts =
  let x = Vector2.x balloon.position in
  let y = Vector2.y balloon.position in
  let x_rate = Vector2.x balloon.velocity in
  let y_rate = Vector2.y balloon.velocity in
  match check_turn_collide balloon turn_pts with
  | None -> balloon.position <- Vector2.create (x +. x_rate) (y +. y_rate)
  | Some turn_pt ->
      balloon.velocity <-
        turn_balloon balloon (if x_rate = 0.0 then y_rate else x_rate) turn_pt;
      move_balloon balloon turn_pts

let rec move_balloons (balloon_list : Balloons.balloon list) turn_pts =
  match balloon_list with
  | [] -> ()
  | h :: t ->
      move_balloon h turn_pts;
      move_balloons t turn_pts

let create_turn_point x1 x2 y1 y2 n =
  ( round_float (x1 *. (!screen_width /. x2)),
    round_float (y1 *. (!screen_height /. y2)),
    round_float n )

let list_from_yojson (dimensions_list : Yojson.Basic.t) =
  match dimensions_list with
  | `List dims -> List.map Yojson.Basic.Util.to_float dims
  | _ -> failwith "impossible"

let produce_point (dim : float list) =
  create_turn_point (List.nth dim 0) (List.nth dim 1) (List.nth dim 2)
    (List.nth dim 3) (List.nth dim 4)

(**Takes in a list of lists.*)
let rec extract_points (rects : Yojson.Basic.t list) =
  match rects with
  | [] -> []
  | rect :: t ->
      let dimensions = list_from_yojson rect in
      produce_point dimensions :: extract_points t

(**Parses the point locations from points.json. The json structure for a
      json containing just one point is as follows:
          \{ "points" : [(x1,x2,y1,y2,n)] *)
let point_json_parse () =
  let json = Yojson.Basic.from_file "./data/points.json" in
  let open Yojson.Basic.Util in
  let point_list = json |> member "points" |> to_list in
  extract_points point_list
