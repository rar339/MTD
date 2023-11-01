let wave1 screen_height : Balloons.balloon list =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst :=
      Balloons.make_redb x (Raylib.Vector2.create 0.0 (screen_height -. 500.))
      :: !balloon_lst
  done;
  !balloon_lst

let wave2 () =
  let balloon_lst = [||] in
  for x = 0 to 15 do
    ignore (Array.append balloon_lst [| Balloons.make_redb x |]);
    ignore (Array.append balloon_lst [| Balloons.make_blueb x |])
  done;
  balloon_lst
