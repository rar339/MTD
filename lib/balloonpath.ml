open Raylib
open Constants
  (*Points are represetned as pairs of ints.*)
  let start_point = (0, 0)

  (*start_point should be changed to be somewhere off the screen*)
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
            (Balloons.get_hitbox (2. *. !screen_height /. 28.) balloon)
          && balloon.current_turn < i
        then (
          balloon.current_turn <- balloon.current_turn + 1;
          Some i)
        else check_turn_collide balloon t

  let turn_balloon rate i =
    match i with
    | 1 -> Vector2.create 0.0 rate
    | 2 -> Vector2.create (-.rate) 0.0
    | 3 -> Vector2.create 0.0 (-.rate)
    | 4 -> Vector2.create rate 0.0
    | 5 -> Vector2.create 0.0 (-.rate)
    | 6 -> Vector2.create rate 0.0
    | 7 -> Vector2.create 0.0 rate
    | 8 -> Vector2.create (-.rate) 0.0
    | 9 -> Vector2.create 0.0 rate
    | 10 -> Vector2.create rate 0.0
    | 11 -> Vector2.create 0.0 (-.rate)
    | _ -> failwith "impossible"

  (*Moves the balloon, taking into consideration if a turn is reached. If a turn
     is reached, changes the velocity but does not update position.*)
  let move_balloon (balloon : Balloons.balloon) turn_pts =
    let x = Vector2.x balloon.position in
    let y = Vector2.y balloon.position in
    let x_rate = Vector2.x balloon.velocity in
    let y_rate = Vector2.y balloon.velocity in
    match check_turn_collide balloon turn_pts with
    | None -> balloon.position <- Vector2.create (x +. x_rate) (y +. y_rate)
    | Some i ->
        balloon.velocity <-
          turn_balloon (if x_rate = 0.0 then y_rate else x_rate) i

  let rec move_balloons (balloon_list : Balloons.balloon list) turn_pts =
    match balloon_list with
    | [] -> ()
    | h :: t ->
        move_balloon h turn_pts;
        move_balloons t turn_pts