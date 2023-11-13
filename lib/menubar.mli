(**This file contains functions that draw and update the menubar.*)

open Raylib
open Bears

val hover_display : bear option ref
(**Hover display is the information about the bears in the menu. This is always
   drawn on top of whatever is currently shown.*)

val select_display : bear option ref
(**Select display represents the menu
   type of a placed bear that is selected.*)

val check_valid_placement : Vector2.t -> Rectangle.t list -> bool
(** Checks for valid placement of bear, contingent on position and cash.
   If a player no longer wants to place a bear, they can move the selected
   choice back to the menu to discard their choice.*)

val check_button_press : Vector2.t -> Rectangle.t list -> bool
(**Checks if a GUI button was pressed, in which case we should not necessarily
   deselect the selected bear.*)

val nevermind : Vector2.t -> Rectangle.t -> bool
(**TODO1: Add spec.*)

val place_bear : unit -> unit
(**TODO2: Add spec.*)

val update_bear_selections : bool -> Vector2.t -> bear list -> unit
(**TODO3: Add spec.*)

val check_click : unit -> unit
(**True corresponds to placed bears (in bear_collection) and false corresponds to
   menu bears (menu_bears).*)

val check_hover : unit -> unit
(**Updates the hover_display to be the current bear hovered over. Could be a menu
   bear OR a placed bear.*)

val lives_box : float -> float -> Rectangle.t
(**TODO4: Add spec.*)

val cash : float -> float -> Rectangle.t
(**TODO5: Add spec.*)

val draw_heart : Texture2D.t option -> float -> float -> unit
(**TODO6: Add spec.*)

val draw_cash : Texture2D.t option -> float -> float -> unit
(**TODO7: Add spec.*)

val lives_and_cash_count : float -> float -> unit
(**TODO8: Add spec.*)

val draw_menu : Rectangle.t -> unit
(**TODO9: Add spec.*)

val draw_range : bear option -> unit
(**Draws the range of the selected bear: grey if it is in a valid location,
      red otherwise.
  Precondition: bear is not a None option.*)

val mem_option : 'a option -> 'a list -> bool

val draw_hover_highlight : unit -> unit
(**Draws the range of the bear if it is hovered over or currently selected.*)

val display_hover_info : bear option -> unit
(**TODO10: Add spec.*)

val draw_info_background : unit -> unit
(**TODO11: Add spec.*)

val draw_info_title : bear_types -> float -> float -> float -> unit
(**TODO12: Add spec.*)

val draw_sell_button : bear -> float -> float -> float -> float -> unit
(**TODO13: Add spec.*)

val display_selection : bear option -> unit
(**Displays the selection GUI for placed bears.*)

val display_bear_info : bear option -> bear option -> unit
(**check_click takes care of updating what should currently be displayed.
   Important: Always draw the select_display before the hover_display.*)
