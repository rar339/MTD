(**This file builds waves of balloons for the player pop with bears.*)

open Balloons
open Yojson.Basic.Util

(**A list of all the waves in our game. Generated in Game.setup().
    Waves are reprsented as a list of tuples of type balloon * int. The ints
    correspond to how many frames before that balloon is spawned (added to 
    current_balloons THUNKY)*)

(**The current wave of baloons, when this is the empty list, the wave is over.*)
let current_wave = ref []

(**The current list of balloons that are actually on the screen.*)
let current_balloons : balloon list ref = ref []

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

let interweave_balloons lst spacing balloon_lst =
  List.iter
    (fun json_string ->
      let color = json_string |> to_string in
      balloon_lst := (balloon_of_string color, spacing) :: !balloon_lst)
    lst

let wave_loop colors count spacing =
  let balloon_lst = ref [] in
  for _ = 0 to count do
    interweave_balloons colors spacing balloon_lst
  done;
  !balloon_lst

let generate_wave n =
  let json = Yojson.Basic.from_file "./data/waves.json" in
  let current_wave_json = json |> member (string_of_int n) |> to_list in
  let ocaml_wave = ref [] in
  List.iter
    (fun json_object ->
      let colors = json_object |> member "Colors" |> to_list in
      let count = json_object |> member "Count" |> to_int in
      let spacing = json_object |> member "Spacing" |> to_int in
      ocaml_wave := wave_loop colors count spacing @ !ocaml_wave)
    current_wave_json;
  !ocaml_wave
