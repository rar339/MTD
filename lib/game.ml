let width = 1100
let height = 720
let count = ref 0
let setup () = ()
let update_game () = ()

let draw_game () =
  let open Raylib in
  begin_drawing ();
  end_drawing ()

(*Active game loop*)
let loop () =
  if !count = 0 then (
    count := 1;
    setup ();
    update_game ();
    draw_game ())
  else update_game ();
  draw_game ()
