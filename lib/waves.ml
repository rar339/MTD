open Balloons
(** This module builds the waves of balloons for each round.*)

(**A list of all the waves in our game. Generated in Game.setup().
    Waves are reprsented as a list of tuples of type balloon * int. The ints
    correspond to how many frames before that balloon is spawned (added to 
    current_balloons THUNKY)*)
let waves : (unit -> (balloon * int) list) list ref = ref []

(**The current wave of baloons, when this is the empty list, the wave is over.*)
let current_wave = ref []

(**The current list of balloons that are actually on the screen.*)
let current_balloons : balloon list ref = ref []

(**"Pops" the next wave from the head of a given list of waves. Used in Game to
   set the current_wave for the next round. *)
let initialize_round (waves : (unit -> (balloon * int) list) list ref) =
  match !waves with
  | [] -> ()
  | h :: t ->
      current_wave := h ();
      waves := t

let wave1 () =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst := (make_balloon x Purple false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave2 () =
  let balloon_lst = ref [] in
  for x = 0 to 40 do
    balloon_lst := (make_balloon x Red false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave3 () =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst := (make_balloon x Red false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Blue false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave4 () =
  let balloon_lst = ref [] in
  for x = 0 to 30 do
    balloon_lst := (make_balloon x Blue false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave5 () =
  let balloon_lst = ref [] in
  for x = 0 to 30 do
    balloon_lst := (make_balloon x Red false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Blue false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave6 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst

(** HERE *)
let wave7 () =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Blue false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave8 () =
  let balloon_lst = ref [] in
  for x = 0 to 40 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;

  for x = 0 to 20 do
    balloon_lst := (make_balloon x Red false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave9 () =
  let balloon_lst = ref [] in
  for x = 0 to 60 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave10 () =
  let balloon_lst = ref [] in
  for x = 0 to 30 do
    balloon_lst := (make_balloon x Yellow false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave11 () =
  let balloon_lst = ref [] in
  for x = 0 to 100 do
    balloon_lst := (make_balloon x Blue false, 5) :: !balloon_lst
  done;
  !balloon_lst

let wave12 () =
  let balloon_lst = ref [] in
  for x = 0 to 50 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Yellow false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave13 () =
  let balloon_lst = ref [] in
  for x = 0 to 30 do
    balloon_lst := (make_balloon x Orange false, 20) :: !balloon_lst
  done;
  !balloon_lst

let wave14 () =
  let balloon_lst = ref [] in
  for x = 0 to 15 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Yellow false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Orange false, 15) :: !balloon_lst;
    balloon_lst := (make_balloon x Yellow false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave15 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Lead true, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave16 () =
  let balloon_lst = ref [] in
  for x = 0 to 50 do
    balloon_lst := (make_balloon x Red false, 2) :: !balloon_lst
  done;
  for x = 0 to 60 do
    balloon_lst := (make_balloon x Blue false, 2) :: !balloon_lst
  done;
  for x = 0 to 40 do
    balloon_lst := (make_balloon x Green false, 2) :: !balloon_lst
  done;
  for x = 0 to 20 do
    balloon_lst := (make_balloon x Green false, 2) :: !balloon_lst
  done;
  !balloon_lst

(********* LEFT TO DO *************)
let wave17 () =
  let balloon_lst = ref [] in
  for x = 0 to 70 do
    balloon_lst := (make_balloon x Yellow false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave18 () =
  let balloon_lst = ref [] in
  for x = 0 to 200 do
    balloon_lst := (make_balloon x Red false, 1) :: !balloon_lst
  done;
  !balloon_lst

let wave19 () =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst := (make_balloon x Lead false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave20 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave21 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave22 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave23 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave24 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave25 () =
  let balloon_lst = ref [] in
  for x = 0 to 10 do
    balloon_lst := (make_balloon x Green false, 15) :: !balloon_lst
  done;
  !balloon_lst
