open Raylib

val turn_points : (int * int * int) list ref
(**Turn points are represented by the type (int * int * int) array. This maps 
    to (x,y,turn number).*)

val check_turn_collide :
  Balloons.balloon -> (int * int * int) list -> (int * int * int) option
(**Checks if the given balloon is colliding with a turn point, meaning it should
   make a turn. *)

val turn_balloon : Balloons.balloon -> float -> int * int * int -> Vector2.t
(**Ensures that no matter what point the balloon collides with the turnpoint, 
it changes direction at the same position. Also, returns the vector of its new velocity as a result of the turn.
    *)

val move_balloon : Balloons.balloon -> (int * int * int) list -> unit
(**Moves the balloon, taking into consideration if a turn is reached. If a turn
   is reached, changes the velocity and then moves the balloon.*)

val move_balloons : Balloons.balloon list -> (int * int * int) list -> unit
(**Updates the positions for balloons in a list of balloons.*)

val create_turn_point :
  float -> float -> float -> float -> float -> int * int * int
(**Creates a turn point given the parameters x1 x2 y1 y2 n.*)

val extract_points : Yojson.Basic.t list -> (int * int * int) list
(**Extracts the turnpoints from a turnpoints json.*)

val point_json_parse : unit -> (int * int * int) list
(**Parses the point locations from points.json. The json structure for a
      json containing just one point is as follows:
          \{ "points" : [(x1,x2,y1,y2,n)] *)
