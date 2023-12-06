(**Generates the rectangles that make the path hitbox.*)

open Raylib

val rect_color : Color.t

val path_rectangles : Rectangle.t list ref
(**The rectangles that make up the hitbox for the path.*)

val draw_rectangles : Rectangle.t list -> unit
(**Draws the rectangles, for debugging.*)

val create_rectangle_bound :
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  Rectangle.t
(**Creates a rectangle given the dimensions.*)

val produce_rectangle : float list -> Rectangle.t
(**Creates a rectangle given a list containing the dimensions.*)

val extract_rectangles : Yojson.Basic.t list -> Rectangle.t list
(**Takes in a list of lists from a json and extracts the path rectangles from the data.*)

val rect_json_parse : unit -> Rectangle.t list
(**Parses the rectangle dimensions from path.json. The json structure for a
json containing just one rectangle is as follows:
    \{ "rects" : [(x1,x2,y1,y2,w1,w2,h1,h2)] *)
