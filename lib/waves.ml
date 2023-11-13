(* This file builds waves of balloons for the player pop with bears. *)
(*A list of all the waves in our game.*)
let waves : (Balloons.balloon * int) list list ref = ref []

(*The current wave of baloons, when this is the empty list, the wave is over*)
let current_wave = ref []

(*The current list of balloons that are actually on the screen.*)
let current_bloons : Balloons.balloon list ref = ref []

let initialize_round (waves : (Balloons.balloon * int) list list ref) =
  match !waves with
  | [] -> ()
  | h :: t ->
      current_wave := h;
      waves := t

let test_wave screen_height =
  [
    ( Balloons.make_balloon 0
        (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.)))
        Purple false,
      15 );
    ( Balloons.make_balloon 0
        (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.)))
        Purple false,
      15 );
  ]

let wave1 screen_height : (Balloons.balloon * int) list =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst :=
      ( Balloons.make_balloon x
          (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.)))
          Red false,
        15 )
      :: !balloon_lst
  done;
  !balloon_lst

let wave2 screen_height =
  let balloon_lst = ref [] in
  for x = 0 to 15 do
    balloon_lst :=
      ( Balloons.make_balloon x
          (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.)))
          Red false,
        15 )
      :: !balloon_lst;
    balloon_lst :=
      ( Balloons.make_balloon x
          (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.)))
          Blue false,
        15 )
      :: !balloon_lst
  done;
  !balloon_lst
