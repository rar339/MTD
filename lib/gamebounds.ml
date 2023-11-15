open Raylib
open Constants

let rect_color = Color.create 0 0 0 100
let path_rectangles : Rectangle.t list ref = ref []

let rec draw_rectangles (rectangles : Rectangle.t list) =
  match rectangles with
  | [] -> ()
  | h :: t ->
      Raylib.draw_rectangle_rec h rect_color;
      draw_rectangles t

let create_rectangle_bound x1 x2 y1 y2 w1 w2 h1 h2 =
  Rectangle.create
    (x1 *. floor (!screen_width /. x2))
    (y1 *. floor (!screen_height /. y2))
    (w1 *. floor (!screen_width /. w2))
    (h1 *. floor (!screen_height /. h2))

let list_from_yojson (dimensions_list : Yojson.Basic.t) =
  match dimensions_list with
  | `List dims -> List.map Yojson.Basic.Util.to_float dims
  | _ -> failwith "impossible"

let produce_rectangle (dim : float list) : Rectangle.t =
  create_rectangle_bound (List.nth dim 0) (List.nth dim 1) (List.nth dim 2)
    (List.nth dim 3) (List.nth dim 4) (List.nth dim 5) (List.nth dim 6)
    (List.nth dim 7)

(*Takes in a list of lists.*)
let rec extract_rectangles (rects : Yojson.Basic.t list) =
  match rects with
  | [] -> []
  | rect :: t ->
      let dimensions = list_from_yojson rect in
      produce_rectangle dimensions :: extract_rectangles t

(**Parses the rectangle dimensions from path.json. The json structure for a
json containing just one rectangle is as follows:
    \{ "rects" : [(x1,x2,y1,y2,w1,w2,h1,h2)] *)
let rect_json_parse () =
  let json = Yojson.Basic.from_file "./data/path.json" in
  let open Yojson.Basic.Util in
  let rect_list = json |> member "rects" |> to_list in
  extract_rectangles rect_list
