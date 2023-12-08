(** This file builds waves of balloons for the player pop with bears. *)

open Balloons

val waves : (unit -> (balloon * int) list) list ref
(**A list of all the waves in our game.*)

val current_wave : (balloon * int) list ref
(**The current wave of baloons, when this is the empty list, the wave is over*)

val current_balloons : balloon list ref
(**The current list of balloons that are actually on the screen.*)

val initialize_round : (unit -> (balloon * int) list) list ref -> unit

val update_wave_speeds : (balloon * 'a) list -> unit
(**Updates the speeds of the balloons in the wave when the speed is updated.*)

val update_balloon_speeds : balloon list -> unit
(**Updates the speeds of the balloons*)

val wave1 : unit -> (balloon * int) list
(**Generates the 1st wave.*)

val wave2 : unit -> (balloon * int) list
(**Generates the 2nd wave.*)

val wave3 : unit -> (Balloons.balloon * int) list
(**Generates the 3rd wave.*)

val wave4 : unit -> (balloon * int) list
(**Generates the 4th wave.*)

val wave5 : unit -> (balloon * int) list
(**Generates the 5th wave.*)

val wave6 : unit -> (Balloons.balloon * int) list
(**Generates the 6th wave.*)

val wave7 : unit -> (balloon * int) list
(**Generates the 7th wave.*)

val wave8 : unit -> (balloon * int) list
(**Generates the 8th wave.*)

val wave9 : unit -> (Balloons.balloon * int) list
(**Generates the 9th wave.*)

val wave10 : unit -> (balloon * int) list
(**Generates the 10th wave.*)

val wave11 : unit -> (balloon * int) list
(**Generates the 11th wave.*)

val wave12 : unit -> (Balloons.balloon * int) list
(**Generates the 12th wave.*)

val wave13 : unit -> (balloon * int) list
(**Generates the 13th wave.*)

val wave14 : unit -> (balloon * int) list
(**Generates the 14th wave.*)

val wave15 : unit -> (Balloons.balloon * int) list
(**Generates the 15th wave.*)

val wave16 : unit -> (balloon * int) list
(**Generates the 16th wave.*)

val wave17 : unit -> (balloon * int) list
(**Generates the 17th wave.*)

val wave18 : unit -> (Balloons.balloon * int) list
(**Generates the 18th wave.*)

val wave19 : unit -> (balloon * int) list
(**Generates the 19th wave.*)

val wave20 : unit -> (balloon * int) list
(**Generates the 20th wave.*)

val wave21 : unit -> (Balloons.balloon * int) list
(**Generates the 21st wave.*)

val wave22 : unit -> (balloon * int) list
(**Generates the 22nd wave.*)

val wave23 : unit -> (balloon * int) list
(**Generates the 23rd wave.*)

val wave24 : unit -> (balloon * int) list
(**Generates the 24th wave.*)

val wave25 : unit -> (Balloons.balloon * int) list
(**Generates the 25th wave.*)

val wave26 : unit -> (Balloons.balloon * int) list
(**Generates the 26th waves. *)

val wave27 : unit -> (balloon * int) list
(**Generates the 27th wave.*)

val wave28 : unit -> (balloon * int) list
(**Generates the 28th wave.*)

val wave29 : unit -> (Balloons.balloon * int) list
(**Generates the 29th wave.*)

val wave30 : unit -> (balloon * int) list
(**Generates the 30th wave.*)