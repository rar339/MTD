let wave1 screen_height : (Balloons.balloon * int) list =
  let balloon_lst = ref [] in
  for x = 0 to 20 do
    balloon_lst :=
      ( Balloons.make_redb x
          (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.))),
        15 )
      :: !balloon_lst
  done;
  !balloon_lst

let wave2 screen_height =
  let balloon_lst = ref [] in
  for x = 0 to 15 do
    balloon_lst :=
      ( Balloons.make_redb x
          (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.))),
        15 )
      :: !balloon_lst;
    balloon_lst :=
      ( Balloons.make_blueb x
          (Raylib.Vector2.create (-30.0) (2. *. floor (!screen_height /. 28.))),
        15 )
      :: !balloon_lst
  done;
  !balloon_lst
