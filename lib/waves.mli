(** This file builds waves of balloons for the player pop with bears. *)

open Balloons

val waves : (unit -> (balloon * int) list) list ref
(**A list of all the waves in our game.*)

val current_wave : (balloon * int) list ref
(**The current wave of baloons, when this is the empty list, the wave is over*)

val current_balloons : balloon list ref
(**The current list of balloons that are actually on the screen.*)

val initialize_round : (unit -> (balloon * int) list) list ref -> unit

val wave1 : unit -> (balloon * int) list
(**Testb documentation*)

val wave2 : unit -> (balloon * int) list
(**Testc documentation*)

val wave3 : unit -> (Balloons.balloon * int) list

(**Tesd doc*)
val wave4 : unit -> (balloon * int) list
(**Testb documentation*)

val wave5 : unit -> (balloon * int) list
(**Testc documentation*)

val wave6 : unit -> (Balloons.balloon * int) list

val wave7 : unit -> (balloon * int) list
(**Testb documentation*)

val wave8 : unit -> (balloon * int) list
(**Testc documentation*)

val wave9 : unit -> (Balloons.balloon * int) list

(**Tesd doc*)
val wave10 : unit -> (balloon * int) list
(**Testb documentation*)

val wave11 : unit -> (balloon * int) list
(**Testc documentation*)

val wave12 : unit -> (Balloons.balloon * int) list

val wave13 : unit -> (balloon * int) list
(**Testb documentation*)

val wave14 : unit -> (balloon * int) list
(**Testc documentation*)

val wave15 : unit -> (Balloons.balloon * int) list

(**Tesd doc*)
val wave16 : unit -> (balloon * int) list
(**Testb documentation*)

val wave17 : unit -> (balloon * int) list
(**Testc documentation*)

val wave18 : unit -> (Balloons.balloon * int) list

val wave19 : unit -> (balloon * int) list
(**Testb documentation*)

val wave20 : unit -> (balloon * int) list
(**Testc documentation*)

val wave21 : unit -> (Balloons.balloon * int) list

(**Tesd doc*)
val wave22 : unit -> (balloon * int) list
(**Testb documentation*)

val wave23 : unit -> (balloon * int) list
(**Testc documentation*)

val wave24 : unit -> (balloon * int) list
(**Testc documentation*)

val wave25 : unit -> (Balloons.balloon * int) list
