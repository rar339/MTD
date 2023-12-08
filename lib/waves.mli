(** This file builds waves of balloons for the player pop with bears. *)

open Balloons

val current_wave : (balloon * int) list ref
(**The current wave of baloons, when this is the empty list, the wave is over*)

val current_balloons : balloon list ref
(**The current list of balloons that are actually on the screen.*)

val update_wave_speeds : (balloon * 'a) list -> unit
(**Updates the speeds of the balloons in the wave when the speed is updated.*)

val update_balloon_speeds : balloon list -> unit
(**Updates the speeds of the balloons*)

val interweave_balloons :
  Yojson.Basic.t list -> 'a -> (balloon * 'a) list ref -> unit
(**Interweaves balloons in a wave as specified by the JSON.*)

val wave_loop : Yojson.Basic.t list -> int -> 'a -> (balloon * 'a) list
(**Given a balloon color, count, and spacing, uses a loop to generate that balloon grouping.*)

val generate_wave : int -> (balloon * int) list
(**Generates the nth wave, as described in the waves JSON file. The JSON format 
    contains mappings for all the waves. Each wave is made up of "loop blocks" 
    which are specified by the color of the balloons generated in that loop 
    (if more then one the are interweaved), the number of balloons of those 
      color(s) generated, and the spacing between the balloons generated.*)
