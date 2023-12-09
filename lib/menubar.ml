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
    (* After selecting a bear from the menu and placing it *)
    (*Following code is logic for a dragon bear. It has its own type of firing*)
    (*The following code is the placement logic for the other types of bears*))
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
  let bear_opt = determine_bear_clicked pos bears in
  let bear_opt2 = Bears.determine_bear_hovered pos bears in
  if placed_bears then select_display := bear_opt2;
  match bear_opt with
  | None -> ()
  | Some bear ->
      if not placed_bears then
        selected_bear :=
          Some (Bears.make_bear false bear.bear_type (get_mouse_position ()))

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
    determine_bear_hovered (get_mouse_position ())
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

let draw_heart screen_width screen_height =
  draw_texture_ex
    (Option.get !heart_logo_img)
    (Vector2.create
       (153. *. screen_width /. 200.)
       (0.44 *. screen_height /. 9.))
    0. 0.12 Color.white

let lives_and_cash_count screen_width screen_height =
  Raygui.label
    (Rectangle.create
       (163. *. screen_width /. 200.)
       (0.7 *. screen_height /. 9.)
       (screen_width /. 100.) (screen_height /. 100.))
    (string_of_int !Constants.lives);
  Raygui.label
    (Rectangle.create
       (182. *. screen_width /. 200.)
       (0.7 *. screen_height /. 9.)
       (screen_width /. 100.) (screen_height /. 100.))
    ("$" ^ string_of_int !Constants.cash)

(**Draws the menubar rectangle and bear icons.*)
let draw_menu rect =
  (* Draws overall white menu *)
  draw_rectangle_rec rect (Color.create 183 201 226 255);
  (* (Color.create 183 201 226 255); *)
  draw_rectangle_lines_ex rect 6. Color.black;
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
    if (Option.get bear).bear_type = Sniper then
      draw_circle
        (Constants.round_float (Vector2.x (Option.get bear).position))
        (Constants.round_float (Vector2.y (Option.get bear).position))
        80. (Color.create 0 0 0 100)
    else
      draw_circle
        (Constants.round_float (Vector2.x (Option.get bear).position))
        (Constants.round_float (Vector2.y (Option.get bear).position))
        (Option.get bear).range (Color.create 0 0 0 100)
  else if (Option.get bear).bear_type = Sniper then
    draw_circle
      (Constants.round_float (Vector2.x (Option.get bear).position))
      (Constants.round_float (Vector2.y (Option.get bear).position))
      80. (Color.create 100 0 0 100)
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
    if (Option.get !hover_display).bear_type = Sniper then
      draw_circle
        (Constants.round_float (Vector2.x (Option.get !hover_display).position))
        (Constants.round_float (Vector2.y (Option.get !hover_display).position))
        80. (Color.create 0 0 0 50)
    else
      draw_circle
        (Constants.round_float (Vector2.x (Option.get !hover_display).position))
        (Constants.round_float (Vector2.y (Option.get !hover_display).position))
        (Option.get !hover_display).range (Color.create 0 0 0 50)

(**Draws the rectangle for the selection GUI.*)
let draw_info_background (beartype : Bears.bear_types) =
  let background_color =
    match beartype with
    | Dart -> Color.create 50 205 50 255
    | Hockey -> Color.create 255 139 86 255
    | Sniper -> Color.gray
    | Polar -> Color.create 115 147 179 255
    | Dragon -> Color.create 163 92 0 255
  in

  draw_rectangle_rec (Option.get !Constants.selection_rect) background_color;
  draw_rectangle_lines_ex (Option.get !Constants.selection_rect) 6. Color.black

let cost_of_beartype beartype =
  let properties = extract_bear_properties beartype in
  properties |> Yojson.Basic.Util.member "cost" |> Yojson.Basic.Util.to_int

let bear_info_text (bear : Bears.bear_types) rect_x rect_y rect_width
    rect_height =
  Raygui.set_font (Option.get !menu_font);
  Raygui.label
    (Rectangle.create rect_x
       (rect_y +. (rect_height /. 6.))
       rect_width (rect_height /. 10.))
    ("Cost : " ^ "$" ^ string_of_int (cost_of_beartype bear));
  match bear with
  | Dart ->
      Raygui.label
        (Rectangle.create rect_x
           (rect_y +. (rect_height /. 3.))
           rect_width (rect_height /. 6.))
        "Shoots darts at balloons. \nCheap and reliable."
  | Hockey ->
      Raygui.label
        (Rectangle.create rect_x rect_y rect_width (2.5 *. rect_height /. 3.))
        " Shoots a barrage of hockey \n pucks in all directions."
  | Polar ->
      Raygui.label
        (Rectangle.create rect_x rect_y rect_width (2.5 *. rect_height /. 3.))
        "Slows down and damages \nballoons. Pairs nicely \nwith hockey bear."
  | Sniper ->
      Raygui.label
        (Rectangle.create rect_x rect_y rect_width (2.5 *. rect_height /. 3.))
        "  Shoots powerful bullets \n\
        \  across map and can pop lead. \n\
        \  Make sure to have one \n\
        \  before Round 19!"
  | Dragon ->
      Raygui.label
        (Rectangle.create rect_x rect_y rect_width (2.5 *. rect_height /. 3.))
        "  Breathes a powerful, lead-\n\
        \  popping, relentless flame \n\
        \  of destruction at balloons. "

(**Draws the title for the selection GUI based on the bear type.*)
let draw_info_title beartype (is_menu_descr : bool) rect_x rect_y rect_width
    rect_height =
  if is_menu_descr then
    bear_info_text beartype rect_x rect_y rect_width rect_height;
  Raygui.set_style (Default `Background_color) 0x6699CC;
  Raygui.set_font (Option.get !game_font);
  Raygui.label
    (Rectangle.create rect_x rect_y rect_width (rect_height /. 5.))
    (if beartype <> Dragon then string_of_beartype beartype ^ " Bear"
     else string_of_beartype beartype)

(**Draws the information about a menu bear that is hovered over, if any. This
    should simply drawn ontop of the current selection, if a bear is selected.*)
let display_hover_info (hover : bear option) =
  if mem_option hover !menu_bears then
    let rect_height = Rectangle.height (Option.get !Constants.selection_rect) in
    let rect_width = Rectangle.width (Option.get !Constants.selection_rect) in
    let rect_x = Rectangle.x (Option.get !Constants.selection_rect) in
    let rect_y = Rectangle.y (Option.get !Constants.selection_rect) in
    match hover with
    | None -> ()
    | Some { bear_type = Dart; _ } ->
        draw_info_background Dart;
        draw_info_title Dart true rect_x rect_y rect_width rect_height
    | Some { bear_type = Hockey; _ } ->
        draw_info_background Hockey;
        draw_info_title Hockey true rect_x rect_y rect_width rect_height
    | Some { bear_type = Polar; _ } ->
        draw_info_background Polar;
        draw_info_title Polar true rect_x rect_y rect_width rect_height
    | Some { bear_type = Sniper; _ } ->
        draw_info_background Sniper;
        draw_info_title Sniper true rect_x rect_y rect_width rect_height
    | Some { bear_type = Dragon; _ } ->
        draw_info_background Dragon;
        draw_info_title Dragon true rect_x rect_y rect_width rect_height

(**Draw the sell button in the selection GUI, the sell rate is 0.70 of the original
    cost.*)
let draw_sell_button bear rect_x rect_y rect_width rect_height =
  Raygui.(set_style (Button `Base_color_focused) 0x80808080);
  Raygui.set_font (Option.get !menu_font);
  let sell_price = Constants.round_float (float_of_int bear.cost *. 0.70) in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_width /. 5.7))
           (rect_y +. (rect_height /. 1.3))
           (rect_width /. 1.6) (rect_height /. 7.))
        ("Sell for:" ^ " $" ^ string_of_int sell_price))
  then (
    bear.sold <- true;
    select_display := None;
    Constants.cash := !Constants.cash + sell_price)

(** Upgrade range button *)
let draw_range_upgrade_button bear rect_x rect_y rect_width rect_height =
  Raygui.set_font (Option.get !menu_font);
  Raygui.(set_style (Button `Base_color_normal) 0xADD8E6);
  Raygui.(set_style (Button `Base_color_pressed) 0x80808080);
  Raygui.(set_style (Button `Base_color_focused) 0x80808080);
  Raygui.(set_style (Button `Border_color_focused) 0x80808080);
  let upgrade_price = Constants.round_float (float_of_int bear.cost *. 0.50) in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_height /. 20.))
           (rect_y +. (rect_height /. 2.0))
           (rect_width /. 1.1) (rect_height /. 5.))
        (if bear.upgrades < 2 then
           "Larger Range \tCost: " ^ string_of_int upgrade_price
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
  Raygui.set_font (Option.get !menu_font);
  Raygui.(set_style (Button `Base_color_normal) 0xADD8E6);
  Raygui.(set_style (Button `Base_color_pressed) 0x80808080);
  Raygui.(set_style (Button `Base_color_focused) 0x80808080);
  Raygui.(set_style (Button `Border_color_focused) 0x80808080);
  let upgrade_price = Constants.round_float (float_of_int bear.cost *. 1.2) in
  let upgrade_text =
    match bear.bear_type with
    | Dart -> "Piercin' Darts "
    | Hockey -> "Piercin' Pucks "
    | Sniper -> "Piercin' Bullets "
    | _ -> ""
  in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_width /. 20.))
           (rect_y +. (rect_height /. 4.0))
           (rect_width /. 1.1) (rect_height /. 5.))
        (if bear.upgrades < 2 then
           upgrade_text ^ "Cost: " ^ string_of_int upgrade_price
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
  Raygui.set_font (Option.get !menu_font);
  Raygui.(set_style (Button `Base_color_normal) 0xADD8E6);
  Raygui.(set_style (Button `Base_color_pressed) 0x80808080);
  Raygui.(set_style (Button `Base_color_focused) 0x80808080);
  Raygui.(set_style (Button `Border_color_focused) 0x80808080);
  let upgrade_price = Constants.round_float (float_of_int bear.cost *. 1.0) in
  if
    Raygui.(
      button
        (Rectangle.create
           (rect_x +. (rect_width /. 20.))
           (rect_y +. (rect_height /. 2.0))
           (rect_width /. 1.1) (rect_height /. 5.))
        (if bear.upgrades < 2 then
           "Faster Speed \tCost: " ^ string_of_int upgrade_price
         else "Cannot Upgrade "))
    && !Constants.cash >= upgrade_price
    && bear.upgrades < 2
  then (
    bear.upgrades <- 1 + bear.upgrades;
    bear.cost <- bear.cost + Constants.round_float (float_of_int upgrade_price);
    bear.attack_speed <-
      bear.attack_speed - int_of_float (0.35 *. float_of_int bear.attack_speed);
    Constants.cash := !Constants.cash - upgrade_price)

(**Displays the selection GUI for placed bears, if a bear is selected.*)
let display_selection selection =
  let rect_width = Rectangle.width (Option.get !Constants.selection_rect) in
  let rect_height = Rectangle.height (Option.get !Constants.selection_rect) in
  let rect_x = Rectangle.x (Option.get !Constants.selection_rect) in
  let rect_y = Rectangle.y (Option.get !Constants.selection_rect) in
  (match selection with
  | None -> ()
  | Some bear when bear.bear_type <> Sniper ->
      draw_circle
        (Constants.round_float (Vector2.x bear.position))
        (Constants.round_float (Vector2.y bear.position))
        bear.range (Color.create 0 0 0 100)
  | _ -> ());
  match selection with
  | None -> ()
  | Some ({ bear_type = Dart; _ } as bear) ->
      draw_info_background Dart;
      draw_info_title Dart false rect_x rect_y rect_width rect_height;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height;
      draw_damage_upgrade_button bear rect_x rect_y rect_width rect_height
  | Some ({ bear_type = Hockey; _ } as bear) ->
      draw_info_background Hockey;
      draw_info_title Hockey false rect_x rect_y rect_width rect_height;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_damage_upgrade_button bear rect_x rect_y rect_width rect_height;
      draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height
  | Some ({ bear_type = Polar; _ } as bear) ->
      draw_info_background Polar;
      draw_info_title Polar false rect_x rect_y rect_width rect_height;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_range_upgrade_button bear rect_x
        (rect_y -. (rect_height /. 4.))
        rect_width rect_height;
      draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height
  | Some ({ bear_type = Sniper; _ } as bear) ->
      draw_info_background Sniper;
      draw_info_title Sniper false rect_x rect_y rect_width rect_height;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height;
      draw_damage_upgrade_button bear rect_x rect_y rect_width rect_height
  | Some ({ bear_type = Dragon; _ } as bear) ->
      draw_info_background Dragon;
      draw_info_title Dragon false rect_x rect_y rect_width rect_height;
      draw_sell_button bear rect_x rect_y rect_width rect_height;
      draw_range_upgrade_button bear rect_x
        (rect_y -. (rect_height /. 4.))
        rect_width rect_height;
      draw_speed_upgrade_button bear rect_x rect_y rect_width rect_height

(**check_click takes care of updating what should currently be displayed.
   Important: Always draw the select_display before the hover_display.*)
let display_bear_info selection hover =
  display_selection selection;
  display_hover_info hover
