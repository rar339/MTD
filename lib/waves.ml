(** This module builds the waves of balloons for each round.*)

open Balloons

(**A list of all the waves in our game. Generated in Game.setup().
    Waves are reprsented as a list of tuples of type balloon * int. The ints
    correspond to how many frames before that balloon is spawned (added to 
    current_balloons)*)
let waves : (balloon * int) list list ref = ref []

(**The current wave of baloons, when this is the empty list, the wave is over.*)
let current_wave = ref []

(**The current list of balloons that are actually on the screen.*)
let current_balloons : balloon list ref = ref []

(**"Pops" the next wave from the head of a given list of waves. Used in Game to
   set the current_wave for the next round. *)
let initialize_round (waves : (balloon * int) list list ref) =
  match !waves with
  | [] -> ()
  | h :: t ->
      current_wave := h;
      waves := t

let test_wave () =
  [
    (Balloons.make_balloon 0 Purple false, 15);
    (Balloons.make_balloon 0 Purple false, 15);
  ]

let wave1 () : (balloon * int) list =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst := (make_balloon x Red false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave2 () =
  let balloon_lst = ref [] in
  for x = 0 to 15 do
    balloon_lst := (make_balloon x Red false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Blue false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave3 () =
  let balloon_lst = ref [] in
  for x = 0 to 15 do
    balloon_lst := (make_balloon x Red false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Lead true, 15) :: !balloon_lst
  done;
  !balloon_lst
