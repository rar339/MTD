(** This file builds waves of balloons for the player pop with bears. *)

open Balloons

val waves : (balloon * int) list list ref
(**A list of all the waves in our game.*)

val current_wave : (balloon * int) list ref
(**The current wave of baloons, when this is the empty list, the wave is over*)

val current_balloons : balloon list ref
(**The current list of balloons that are actually on the screen.*)

val initialize_round : (balloon * int) list list ref -> unit
(**Test documentation*)

val test_wave : float ref -> (balloon * int) list
(**Testa documentation*)

val wave1 : float ref -> (balloon * int) list
(**Testb documentation*)

val wave2 : float ref -> (balloon * int) list
(**Testc documentation*)

val wave3 : float ref -> (Balloons.balloon * int) list
(**Tesd doc*)
