let wave1 () : Balloons.balloon list =
  let balloon_lst = [] in
  for x = 0 to 20 do
    ignore (Balloons.make_redb x :: balloon_lst)
  done;
  balloon_lst
  done;
  balloon_lst

let wave2 () =
  let balloon_lst = [||] in
  for x = 0 to 15 do
    ignore (Array.append balloon_lst [| Balloons.make_redb x |]);
    ignore (Array.append balloon_lst [| Balloons.make_blueb x |])
  done;
  balloon_lst
