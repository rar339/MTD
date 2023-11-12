open Raylib
open Bears
open Constants
open Gamebounds

(*Hover display is the information about the bears in the menu. This is always
   drawn on top of whatever is currently shown. Select display represents the menu
   type of a placed bear that is selected.*)
let hover_display : bear option ref = ref None
let select_display : bear option ref = ref None

(* Checks for valid placement of bear, contingent on position and cash.
   If a player no longer wants to place a bear, they can move the selected
   choice back to the menu to discard their choice.

   The radius of the circle in check_collision_circle_rec will determine how much
   the bears can overhang on path.*)
let rec check_valid_placement (mouse_pos : Vector2.t)
    (rectangle_bounds : Rectangle.t list) =
  match rectangle_bounds with
  | [] -> true
  | rect :: t ->
      (not (check_collision_circle_rec mouse_pos 15.0 rect))
      && check_valid_placement mouse_pos t

let nevermind (mouse_pos : Vector2.t) (menu : Rectangle.t) =
  check_collision_point_rec mouse_pos menu

let place_bear () =
  if !selected = false && !selected_bear <> None then selected := true
  else if
    nevermind (get_mouse_position ()) (Option.get !Constants.menu_rect)
    && !selected = true
    && is_mouse_button_pressed Left
  then (
    selected := false;
    selected_bear := None)
  else if
    !selected = true
    && is_mouse_button_pressed Left
    && check_valid_placement (get_mouse_position ()) !path_rectangles
    && (Option.get !selected_bear).cost <= !Constants.cash
    && Bears.check_collision_bears !selected_bear !Bears.bear_collection
       == false
  then (
    selected := false;
    Bears.bear_collection := Option.get !selected_bear :: !Bears.bear_collection;
    Constants.cash := !Constants.cash - (Option.get !selected_bear).cost;
    selected_bear := None)

let update_bear_selections placed_bears pos bears =
  let bear = determine_bear_clicked pos bears in
  if placed_bears then select_display := bear;

  match bear with
  | None -> ()
  | Some { bear_type = Dart; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_dart_bear (get_mouse_position ()))
  | Some { bear_type = Hockey; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_hockey_bear (get_mouse_position ()))
  | Some { bear_type = Pumpkin; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_pumpkin_bear (get_mouse_position ()))
  | Some { bear_type = Ezra; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_ezra_bear (get_mouse_position ()))
  | Some { bear_type = Dragon; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_dragon_bear (get_mouse_position ()))

(*True corresponds to placed bears (in bear_collection) and false corresponds to
   menu bears (menu_bears).*)
let check_click () =
  if is_mouse_button_pressed Left then (
    update_bear_selections true (get_mouse_position ()) !bear_collection;
    update_bear_selections false (get_mouse_position ()) !menu_bears)

(*Updates the hover_display to be the current bear hovered over. Could be a menu
   bear OR a placed bear.*)
let check_hover () =
  hover_display :=
    determine_bear_clicked (get_mouse_position ())
      (!menu_bears @ !bear_collection)

let lives_box screen_width screen_height =
  Rectangle.create
    (149. *. screen_width /. 200.)
    (0.5 *. screen_height /. 9.)
    (1. *. screen_width /. 9.)
    (screen_height /. 19.)

let cash screen_width screen_height =
  Rectangle.create
    (173. *. screen_width /. 200.)
    (0.5 *. screen_height /. 9.)
    (1. *. screen_width /. 9.)
    (screen_height /. 19.)

let draw_heart heart_text screen_width screen_height =
  draw_texture_ex (Option.get heart_text)
    (Vector2.create
       (149. *. screen_width /. 200.)
       (0.45 *. screen_height /. 9.))
    0. 0.10 Color.white

let draw_cash cash_text screen_width screen_height =
  draw_texture_ex (Option.get cash_text)
    (Vector2.create
       (174. *. screen_width /. 200.)
       (0.55 *. screen_height /. 9.))
    0. 0.07 Color.white

let lives_and_cash_count screen_width screen_height =
  Raylib.draw_text
    (string_of_int !Constants.lives)
    (int_of_float (158. *. screen_width /. 200.))
    (int_of_float (0.62 *. screen_height /. 9.))
    25 Color.white;
  Raylib.draw_text
    (string_of_int !Constants.cash)
    (int_of_float (182. *. screen_width /. 200.))
    (int_of_float (0.62 *. screen_height /. 9.))
    25 Color.white

let draw_menu rect =
  draw_rectangle_rec rect (Color.create 183 201 226 255);
  draw_rectangle_lines_ex rect 3. Color.black;
  (* Draw the menu bears *)
  Bears.draw_bears !menu_bears

(*Selection GUI****************************************************************)
(*Draws the range of the selected bear: grey if it is in a valid location,
      red otherwise.
  Precondition: bear is not a Nonse option.*)
let draw_range bear =
  if
    check_valid_placement (get_mouse_position ()) !path_rectangles
    && Bears.check_collision_bears bear !Bears.bear_collection == false
  then
    draw_circle
      (Constants.round_float (Vector2.x (Option.get bear).position))
      (Constants.round_float (Vector2.y (Option.get bear).position))
      (Option.get bear).range (Color.create 0 0 0 100)
  else
    draw_circle
      (Constants.round_float (Vector2.x (Option.get bear).position))
      (Constants.round_float (Vector2.y (Option.get bear).position))
      (Option.get bear).range (Color.create 100 0 0 100)

let mem_option bear_opt bear_lst =
  match bear_opt with
  | None -> false
  | _ -> List.mem (Option.get bear_opt) bear_lst

(*Displays the selection GUI for placed bears.*)
let display_selection selection =
  match selection with
  | None -> ()
  | Some ({ bear_type = Dart; _ } as bear) -> print_int bear.attack_speed
  | Some ({ bear_type = Hockey; _ } as bear) -> print_int bear.attack_speed
  | Some ({ bear_type = Pumpkin; _ } as bear) -> print_int bear.attack_speed
  | Some ({ bear_type = Ezra; _ } as bear) -> print_int bear.attack_speed
  | Some ({ bear_type = Dragon; _ } as bear) -> print_int bear.attack_speed

(*Draws the range of the bear if it is hovered over or currently selected.*)
let draw_hover_highlight () =
  if mem_option !hover_display !bear_collection && !hover_display <> None then
    draw_circle
      (Constants.round_float (Vector2.x (Option.get !hover_display).position))
      (Constants.round_float (Vector2.y (Option.get !hover_display).position))
      (Option.get !hover_display).range (Color.create 0 0 0 50);
  if !select_display <> None then
    draw_circle
      (Constants.round_float (Vector2.x (Option.get !select_display).position))
      (Constants.round_float (Vector2.y (Option.get !select_display).position))
      (Option.get !select_display).range (Color.create 0 0 0 50)

let display_hover_info (hover : bear option) =
  if mem_option hover !menu_bears then
    match hover with
    | None -> ()
    | Some ({ bear_type = Dart; _ } as bear) -> print_int bear.attack_speed
    | Some ({ bear_type = Hockey; _ } as bear) -> print_int bear.attack_speed
    | Some ({ bear_type = Pumpkin; _ } as bear) -> print_int bear.attack_speed
    | Some ({ bear_type = Ezra; _ } as bear) -> print_int bear.attack_speed
    | Some ({ bear_type = Dragon; _ } as bear) -> print_int bear.attack_speed

(*check_click takes care of updating what should currently be displayed.
   Important: Always draw the select_display before the hover_display.*)
let display_bear_info selection hover =
  display_selection selection;
  display_hover_info hover
