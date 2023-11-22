(**This module contains functions for drawing and updating the menubar.*)

open Raylib
open Bears
open Constants
open Gamebounds

(**This value represents the bear that is currently hovered over, if any.*)
let hover_display : bear option ref = ref None

(**Select display represents the menu
   type of a placed bear that is selected.*)
let select_display : bear option ref = ref None

(** Checks for valid placement of bear, contingent on position and cash.
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

(**Checks if a GUI button was pressed, in which case we should not necessarily
   deselect the selected bear.*)
let rec check_button_press click_pos rect_lst =
  match rect_lst with
  | [] -> false
  | rect :: rest ->
      check_collision_point_rec click_pos rect
      || check_button_press click_pos rest

(**Checks if the user has clicked the menu. This allows them to put a bear they
    have picked up back into the menu.*)
let nevermind (mouse_pos : Vector2.t) (menu : Rectangle.t) =
  check_collision_point_rec mouse_pos menu

(**Handles picking up and placing bears.*)
let place_bear () =
  (* When first selecting a bear *)
  if !selected = false && !selected_bear <> None then selected := true
    (* After selecting a bear from the menu but wanting to return it *)
  else if
    nevermind (get_mouse_position ()) (Option.get !Constants.menu_rect)
    && !selected = true
    && is_mouse_button_pressed Left
  then (
    selected := false;
    selected_bear := None
    (* After selecting a bear from the menu and placing it *))
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
  else if !selected = true then selected := true

(**Checks if the current mouse position corresponds to a bear in the given list.
  placed_bear represents whether or not he passed in list is a lsit of placed 
  bears or menu bears.
  Parameters: [placed_bears] [position] [bear_list]*)
let update_bear_selections placed_bears pos bears =
  let bear = determine_bear_clicked pos bears in
  if placed_bears then select_display := bear;
  match bear with
  | None -> ()
  | Some { bear_type = Dart; _ } ->
      if not placed_bears then
        selected_bear :=
          Some (Bears.make_dart_bear false (get_mouse_position ()))
  | Some { bear_type = Hockey; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_hockey_bear (get_mouse_position ()))
  | Some { bear_type = Pumpkin; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_pumpkin_bear (get_mouse_position ()))
  | Some { bear_type = Sniper; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_sniper_bear (get_mouse_position ()))
  | Some { bear_type = Dragon; _ } ->
      if not placed_bears then
        selected_bear := Some (Bears.make_dragon_bear (get_mouse_position ()))

(**Check_click is called in Game.update_game() to update whether or not a bear
    has been selected. If a user clicked a button, we do not deselect their 
    current selection.*)
let check_click () =
  let final_rect =
    match !selection_rect with
    | Some r -> r
    | None -> Rectangle.create 0. 0. 0. 0.
  in
  if
    check_button_press (get_mouse_position ())
      [
        final_rect;
        Rectangle.create
          (149. *. !screen_width /. 200.)
          (8. *. !screen_height /. 9.)
          (2. *. !screen_width /. 9.)
          (!screen_height /. 19.);
      ]
  then ()
  else if is_mouse_button_pressed Left then (
    update_bear_selections true (get_mouse_position ()) !bear_collection;
    update_bear_selections false (get_mouse_position ()) !menu_bears)

(**Updates the hover_display to be the current bear hovered over. Could be a menu
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

(**Draws the menubar rectangle and bear icons.*)
let draw_menu rect =
  (* Draws overall white menu *)
  draw_rectangle_rec rect (Color.create 183 201 226 255);
  draw_rectangle_lines_ex rect 3. Color.black;
  (* Draw the menu bears *)
  Bears.draw_menu_bears !menu_bears

(*Selection GUI****************************************************************)

(**Draws the range of the selected bear: grey if it is in a valid location,
      red otherwise.
  Precondition: bear is not a None option.*)
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

(**Draws the range of the bear if it is hovered over or currently selected.*)
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

(**Draws the rectangle for the selection GUI.*)
let draw_info_background () =
  draw_rectangle_rec (Option.get !Constants.selection_rect) Color.gold;
  draw_rectangle_lines_ex (Option.get !Constants.selection_rect) 3. Color.black

(**Draws the title for the selection GUI based on the bear type.*)
let draw_info_title beartype rect_x rect_y rect_width =
  draw_text_ex (Option.get !game_font)
    (Bears.string_of_beartype beartype ^ " Bear")
    (Vector2.create (rect_x +. (0.9 *. (rect_width /. 3.))) (rect_y *. 1.05))
    45. 2. Color.black

(**Draws the information about a menu bear that is hovered over, if any. This
    should simply drawn ontop of the current selection, if a bear is selected.*)
let display_hover_info (hover : bear option) =
  if mem_option hover !menu_bears then
    match hover with
    | None -> ()
    | Some ({ bear_type = Dart; _ } as bear) ->
        bear.position <- bear.position;
        draw_info_background ()
    | Some ({ bear_type = Hockey; _ } as bear) ->
        bear.position <- bear.position;
        draw_info_background ()
    | Some ({ bear_type = Pumpkin; _ } as bear) ->
        bear.position <- bear.position;
        draw_info_background ()
    | Some ({ bear_type = Sniper; _ } as bear) ->
        bear.position <- bear.position;
        draw_info_background ()
    | Some ({ bear_type = Dragon; _ } as bear) ->
        bear.position <- bear.position;
        draw_info_background ()

(**Draw the sell button in the selection GUI, the sell rate is 0.70 of the original
    cost.*)
let draw_sell_button bear rect_x rect_y rect_width rect_height =
  let sell_price = Constants.round_float (float_of_int bear.cost *. 0.70) in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_width /. 4.))
           (rect_y +. (rect_height /. 1.15))
           (rect_width /. 2.) (rect_height /. 10.))
        ("Sell: " ^ string_of_int sell_price))
  then (
    bear.sold <- true;
    select_display := None;
    Constants.cash := !Constants.cash + sell_price)

(** Upgrade range button *)
let draw_range_upgrade_button bear rect_x rect_y rect_width rect_height =
  let upgrade_price = Constants.round_float (float_of_int bear.cost *. 0.50) in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_width /. 4.))
           (rect_y +. (rect_height /. 2.0))
           (rect_width /. 2.) (rect_height /. 5.))
        (if bear.upgrades < 2 then
           "Larger Range \n        Cost: " ^ string_of_int upgrade_price
         else "Cannot Upgrade "))
    && !Constants.cash >= upgrade_price
    && bear.upgrades < 2
  then (
    bear.upgrades <- 1 + bear.upgrades;
    bear.cost <- bear.cost + Constants.round_float (float_of_int upgrade_price);
    bear.range <- bear.range +. (bear.range *. 0.2);
    Constants.cash := !Constants.cash - upgrade_price)

(** Upgrade damage button *)
let draw_damage_upgrade_button bear rect_x rect_y rect_width rect_height =
  let upgrade_price = Constants.round_float (float_of_int bear.cost *. 0.75) in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_width /. 4.))
           (rect_y +. (rect_height /. 4.0))
           (rect_width /. 2.) (rect_height /. 5.))
        (if bear.upgrades < 2 then
           "Piercing Ballons \n        Cost: " ^ string_of_int upgrade_price
         else "Cannot Upgrade "))
    && !Constants.cash >= upgrade_price
    && bear.upgrades < 2
  then (
    bear.upgrades <- 1 + bear.upgrades;
    bear.cost <- bear.cost + Constants.round_float (float_of_int upgrade_price);
    bear.damage <- bear.damage + 1;
    Constants.cash := !Constants.cash - upgrade_price)

(** Upgrade attack speed button *)
let draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height =
  let upgrade_price = Constants.round_float (float_of_int bear.cost *. 0.50) in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_width /. 4.))
           (rect_y +. (rect_height /. 4.0))
           (rect_width /. 2.) (rect_height /. 5.))
        (if bear.upgrades < 2 then
           "Faster Speed \n        Cost: " ^ string_of_int upgrade_price
         else "Cannot Upgrade "))
    && !Constants.cash >= upgrade_price
    && bear.upgrades < 2
  then (
    bear.upgrades <- 1 + bear.upgrades;
    bear.cost <- bear.cost + Constants.round_float (float_of_int upgrade_price);
    bear.attack_speed <- bear.attack_speed - 10;
    Constants.cash := !Constants.cash - upgrade_price)

(**Displays the selection GUI for placed bears, if a bear is selected.*)
let display_selection selection =
  let rect_width = Rectangle.width (Option.get !Constants.selection_rect) in
  let rect_height = Rectangle.height (Option.get !Constants.selection_rect) in
  let rect_x = Rectangle.x (Option.get !Constants.selection_rect) in
  let rect_y = Rectangle.y (Option.get !Constants.selection_rect) in
  match selection with
  | None -> ()
  | Some ({ bear_type = Dart; _ } as bear) ->
      draw_info_background ();
      draw_info_title Dart rect_x rect_y rect_width;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_range_upgrade_button bear rect_x rect_y rect_width rect_height;
      draw_damage_upgrade_button bear rect_x rect_y rect_width rect_height
  | Some ({ bear_type = Hockey; _ } as bear) ->
      draw_info_background ();
      draw_info_title Hockey rect_x rect_y rect_width;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_range_upgrade_button bear rect_x rect_y rect_width rect_height;
      draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height
  | Some ({ bear_type = Pumpkin; _ } as bear) -> print_int bear.attack_speed
  | Some ({ bear_type = Sniper; _ } as bear) ->
      draw_info_background ();
      draw_info_title Sniper rect_x rect_y rect_width;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_range_upgrade_button bear rect_x rect_y rect_width rect_height;
      draw_damage_upgrade_button bear rect_x rect_y rect_width rect_height
  | Some ({ bear_type = Dragon; _ } as bear) ->
      draw_info_background ();
      draw_info_title Dragon rect_x rect_y rect_width;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_range_upgrade_button bear rect_x rect_y rect_width rect_height;
      draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height

(**check_click takes care of updating what should currently be displayed.
   Important: Always draw the select_display before the hover_display.*)
let display_bear_info selection hover =
  display_selection selection;
  display_hover_info hover
