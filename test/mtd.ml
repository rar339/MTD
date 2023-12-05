open OUnit2
open MTD

let vel0 = Raylib.Vector2.create 0.0 0.0

(* MAKE BALLOON TO TEST *)
let make_test_balloon x_pos y_pos current_turn velocity color is_lead :
    Balloons.balloon =
  {
    color;
    velocity;
    position = Raylib.Vector2.create x_pos y_pos;
    is_lead;
    img = Raylib.load_texture "./img/balloons/red.png";
    current_turn;
    remove = false;
    is_slowed = false;
    slow_counter = 0;
  }

let triple_option_printer elt_printer triple =
  match triple with
  | None -> "None"
  | Some (x, y, i) ->
      "(" ^ elt_printer x ^ "," ^ elt_printer y ^ "," ^ elt_printer i ^ ")"

(* Builds a window so that the tests can run. *)
let run_tests () =
  Raylib.init_window 0 0 "TESTS";
  (*Lead Balloon at (0,0) with 0 with no velocity, and is lead. *)
  let balloon1 = make_test_balloon 0.0 0.0 0 vel0 Lead true in
  let check_turn_collide_tests =
    [
      ( "balloonpath check_turn_collide empty list" >:: fun _ ->
        assert_equal (Balloonpath.check_turn_collide balloon1 []) None );
      ( "balloonpath check_turn_collide single element list, no collide"
      >:: fun _ ->
        assert_equal None
          (MTD.Balloonpath.check_turn_collide balloon1 [ (1000, 2000, 4) ]) );
      ( "balloonpath check_turn_collide single element list, yes collide"
      >:: fun _ ->
        assert_equal
          ~printer:(triple_option_printer string_of_int)
          (Some (0, 0, 4))
          (MTD.Balloonpath.check_turn_collide balloon1 [ (0, 0, 4) ]) );
      ( "balloonpath check_turn_collide multi element list, yes collide"
      >:: fun _ ->
        assert_equal
          ~printer:(triple_option_printer string_of_int)
          (Some (0, 0, 4))
          (MTD.Balloonpath.check_turn_collide balloon1
             [ (0, 0, 4); (100, 200, 5); (3456, 345, 0) ]) );
    ]
  in
  let turn_balloon_tests = [] in
  let move_balloons_tests = [] in
  let extract_points_tests = [] in
  let point_json_parse_tests = [] in

  let balloonpath_tests =
    List.flatten
      [
        check_turn_collide_tests @ turn_balloon_tests @ move_balloons_tests
        @ extract_points_tests @ point_json_parse_tests;
      ]
  in

  (* FINISH THESE LATER *)
  (* let bears_tests = []
     let constants_tests = []
     let game_tests = []
     let gamebackground_tests = []
     let gamebounds_tests = []
     let menubar_tests = []
     let projectiles_tests = []
     let waves_tests = [] *)
  let balloons_tests =
    [ ("balloon is_lead test" >:: fun _ -> assert_equal true balloon1.is_lead) ]
  in

  let tests = balloonpath_tests @ balloons_tests in
  let suite = "test suite for MTD" >::: List.flatten [ tests ] in

  run_test_tt_main suite;

  Raylib.close_window ()

let () = run_tests ()
