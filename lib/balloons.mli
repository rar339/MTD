(** This file specifies the properties and behaviors of balloons. There
   are several different colors of balloons, each with their own velocities
   and associated values. *)

type balloon_colors =
  | None
  | Red
  | Blue
  | Green
  | Orange
  | Purple
  | Yellow
  | Lead

(* Maps a balloon to an integer value. *)
val balloon_value : balloon_colors -> int

type balloon = {
  mutable color : balloon_colors;
  mutable velocity : Raylib.Vector2.t;
  mutable position : Raylib.Vector2.t;
  mutable next_down : balloon_colors;
  mutable is_lead : bool;
  mutable img : Raylib.Texture2D.t;
  mutable current_turn : int;
  mutable remove : bool;
  order : int;
}

val setup_hitbox : float -> unit

val get_hitbox : balloon -> Raylib.Rectangle.t
(** This should be dependent on the size of the balloon image. Needs fine tuning. *)

val draw_balloons : float -> balloon list -> unit
(** Draws balloons in a balloon list. *)

(** Creates a balloon given the position and color. *)

val make_balloon : int -> Raylib.Vector2.t -> balloon_colors -> bool -> balloon

val lower_lives : balloon_colors -> unit
(**Lowers player lives when a balloon crosses the finish line based on the
      value of that balloon. *)

val lower_layer : balloon -> unit
(**Modifies the given balloon to be one layer color 'lower'. If the balloon
      is red, sets balloon.remove to true.*)

(***Modifies the given balloon to be the correct layer color based on the damage
   of the projectile.*)
val hit_update : int -> balloon -> unit

val remove_balloons : balloon list -> balloon list
(** Removes a balloon if it has crossed the finish line and reduces a player's
      lives by calling lower_lives. *)
