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

let rec update_wave_speeds cur_wave =
  match cur_wave with
  | (bln, _) :: t ->
      bln.velocity <- change_velocity bln bln.color;
      update_wave_speeds t
  | [] -> ()

let rec update_balloon_speeds cur_blns =
  match cur_blns with
  | bln :: t ->
      bln.velocity <- change_velocity bln bln.color;
      update_balloon_speeds t
  | [] -> ()

let wave1 () =
  let balloon_lst = ref [] in
  for _ = 0 to 20 do
    balloon_lst := (make_balloon Red, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave2 () =
  let balloon_lst = ref [] in
  for _ = 0 to 30 do
    balloon_lst := (make_balloon Red, 10) :: !balloon_lst
  done;
  !balloon_lst

let wave3 () =
  let balloon_lst = ref [] in
  for _ = 0 to 10 do
    balloon_lst := (make_balloon Red, 15) :: !balloon_lst;
    balloon_lst := (make_balloon Blue, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave4 () =
  let balloon_lst = ref [] in
  for _ = 0 to 25 do
    balloon_lst := (make_balloon Blue, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave5 () =
  let balloon_lst = ref [] in
  for _ = 0 to 30 do
    balloon_lst := (make_balloon Red, 15) :: !balloon_lst;
    balloon_lst := (make_balloon Blue, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave6 () =
  let balloon_lst = ref [] in
  for _ = 0 to 15 do
    balloon_lst := (make_balloon Green, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave7 () =
  let balloon_lst = ref [] in
  for _ = 0 to 20 do
    balloon_lst := (make_balloon Green, 15) :: !balloon_lst;
    balloon_lst := (make_balloon Blue, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave8 () =
  let balloon_lst = ref [] in
  for _ = 0 to 30 do
    balloon_lst := (make_balloon Green, 15) :: !balloon_lst
  done;

  for _ = 0 to 20 do
    balloon_lst := (make_balloon Red, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave9 () =
  let balloon_lst = ref [] in
  for _ = 0 to 40 do
    balloon_lst := (make_balloon Green, 40) :: !balloon_lst
  done;
  !balloon_lst

let wave10 () =
  let balloon_lst = ref [] in
  for _ = 0 to 15 do
    balloon_lst := (make_balloon Yellow, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave11 () =
  let balloon_lst = ref [] in
  for _ = 0 to 100 do
    balloon_lst := (make_balloon Blue, 5) :: !balloon_lst
  done;
  !balloon_lst

let wave12 () =
  let balloon_lst = ref [] in
  for _ = 0 to 30 do
    balloon_lst := (make_balloon Green, 15) :: !balloon_lst
  done;
  for _ = 0 to 10 do
    balloon_lst := (make_balloon Yellow, 50) :: !balloon_lst
  done;
  !balloon_lst

let wave13 () =
  let balloon_lst = ref [] in
  for _ = 0 to 22 do
    balloon_lst := (make_balloon Yellow, 20) :: !balloon_lst
  done;
  !balloon_lst

let wave14 () =
  let balloon_lst = ref [] in
  for _ = 0 to 12 do
    balloon_lst := (make_balloon Green, 15) :: !balloon_lst;
    balloon_lst := (make_balloon Yellow, 15) :: !balloon_lst;
    balloon_lst := (make_balloon Green, 15) :: !balloon_lst;
    balloon_lst := (make_balloon Yellow, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave15 () =
  let balloon_lst = ref [] in
  for _ = 0 to 50 do
    balloon_lst := (make_balloon Green, 10) :: !balloon_lst
  done;
  !balloon_lst

let wave16 () =
  let balloon_lst = ref [] in
  for _ = 0 to 50 do
    balloon_lst := (make_balloon Red, 2) :: !balloon_lst
  done;
  for _ = 0 to 60 do
    balloon_lst := (make_balloon Blue, 2) :: !balloon_lst
  done;
  for _ = 0 to 40 do
    balloon_lst := (make_balloon Green, 2) :: !balloon_lst
  done;
  for _ = 0 to 20 do
    balloon_lst := (make_balloon Green, 2) :: !balloon_lst
  done;
  for _ = 0 to 10 do
    balloon_lst := (make_balloon Yellow, 30) :: !balloon_lst
  done;
  !balloon_lst

(********* LEFT TO DO *************)
let wave17 () =
  let balloon_lst = ref [] in
  for _ = 0 to 10 do
    balloon_lst := (make_balloon Orange, 25) :: !balloon_lst
  done;
  !balloon_lst

let wave18 () =
  let balloon_lst = ref [] in
  for _ = 0 to 150 do
    balloon_lst := (make_balloon Blue, 1) :: !balloon_lst
  done;
  !balloon_lst

let wave19 () =
  let balloon_lst = ref [] in
  for _ = 0 to 1 do
    balloon_lst := (make_balloon Lead, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave20 () =
  let balloon_lst = ref [] in
  for _ = 0 to 15 do
    balloon_lst :=
      (make_balloon Orange, 15) :: (make_balloon Yellow, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave21 () =
  let balloon_lst = ref [] in
  for _ = 0 to 50 do
    balloon_lst := (make_balloon Yellow, 15) :: !balloon_lst
  done;
  !balloon_lst

let wave22 () =
  let balloon_lst = ref [] in
  for _ = 0 to 10 do
    balloon_lst :=
      (make_balloon Green, 15)
      :: (make_balloon Yellow, 5)
      :: (make_balloon Orange, 1)
      :: !balloon_lst
  done;
  !balloon_lst

let wave23 () =
  let balloon_lst = ref [] in
  for _ = 0 to 20 do
    balloon_lst := (make_balloon Purple, 20) :: !balloon_lst
  done;
  !balloon_lst

let wave24 () =
  let balloon_lst = ref [] in
  for _ = 0 to 30 do
    balloon_lst := (make_balloon Orange, 2) :: !balloon_lst
  done;
  for _ = 0 to 100 do
    balloon_lst := (make_balloon Green, 2) :: !balloon_lst
  done;
  !balloon_lst

let wave25 () =
  let balloon_lst = ref [] in
  for _ = 0 to 15 do
    balloon_lst :=
      (make_balloon Lead, 50) :: (make_balloon Purple, 15) :: !balloon_lst
  done;
  !balloon_lst
