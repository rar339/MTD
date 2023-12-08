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
    freeze_duration = 0;
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
      ( "balloonpath check_turn_collide multi element list, no collide"
      >:: fun _ ->
        assert_equal
          ~printer:(triple_option_printer string_of_int)
          None
          (MTD.Balloonpath.check_turn_collide balloon1
             [ (100, 200, 5); (100, 200, 5); (3456, 345, 0) ]) );
      ( "balloonpath check_turn_collide multi element list, yes collide"
      >:: fun _ ->
        assert_equal
          ~printer:(triple_option_printer string_of_int)
          (Some (0, 0, 4))
          (MTD.Balloonpath.check_turn_collide balloon1
             [ (0, 0, 4); (100, 200, 5); (3456, 345, 0) ]) );
    ]
  in
  let turn_balloon_tests =
    let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
    [
      (let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 1.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 1)
       in
       "balloonpath turn_balloon multi element list, yes turn 1" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create (-1.0) 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 2)
       in
       "balloonpath turn_balloon multi element list, yes turn 2" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 (-1.0) in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 3)
       in
       "balloonpath turn_balloon multi element list, yes turn 3" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 1.0 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 4)
       in
       "balloonpath turn_balloon multi element list, yes turn 4" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 (-1.0) in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 5)
       in
       "balloonpath turn_balloon multi element list, yes turn 5" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 1.0 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 6)
       in
       "balloonpath turn_balloon multi element list, yes turn 6" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 1.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 7)
       in
       "balloonpath turn_balloon multi element list, yes turn 7" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create (-1.0) 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 8)
       in
       "balloonpath turn_balloon multi element list, yes turn 8" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 1.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 9)
       in
       "balloonpath turn_balloon multi element list, yes turn 9" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 1.0 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 10)
       in
       "balloonpath turn_balloon multi element list, yes turn 10" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead true in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 (-1.0) in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 11)
       in
       "balloonpath turn_balloon multi element list, yes turn 11" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
    ]
  in
  let move_balloons_tests =
    [
      ( "balloonpath move_balloons empty balloons and turn points" >:: fun _ ->
        assert_equal () (MTD.Balloonpath.move_balloons [] []) );
      (let balloon_lst =
         [
           make_test_balloon 1.0 1.0 1 vel0 Lead true;
           make_test_balloon 1.0 1.0 1 vel0 Red false;
         ]
       in
       "balloonpath move_balloons multi element balloons empty turn points"
       >:: fun _ ->
       assert_equal () (MTD.Balloonpath.move_balloons balloon_lst []));
      (let turn_points = [ (1, 1, 2); (2, 3, 4) ] in
       "balloonpath move_balloons empty balloons with turn points" >:: fun _ ->
       assert_equal () (MTD.Balloonpath.move_balloons [] turn_points));
      (let balloon_lst =
         [
           make_test_balloon 1.0 1.0 1 vel0 Lead true;
           make_test_balloon 1.0 1.0 1 vel0 Red false;
         ]
       in
       let turn_points = [ (1, 1, 2); (2, 3, 4) ] in
       "balloonpath move_balloons multi element balloons and turn points"
       >:: fun _ ->
       assert_equal () (MTD.Balloonpath.move_balloons balloon_lst turn_points));
    ]
  in
  let create_turn_point_test =
    [
      ( "balloonpath create_turn_point all zeros" >:: fun _ ->
        assert_equal (0, 0, 0)
          (MTD.Balloonpath.create_turn_point 0.0 0.0 0.0 0.0 0.0) );
      ( "balloonpath create_turn_point actual point" >:: fun _ ->
        assert_equal (1194, 834, 1)
          (MTD.Balloonpath.create_turn_point 1.0 1.0 10.0 10.0 1.0) );
    ]
  in
  let produce_point_tests =
    [
      ( "balloonpath produce_point single point" >:: fun _ ->
        assert_equal (1194, 834, 1)
          (MTD.Balloonpath.produce_point [ 1.0; 1.0; 1.0; 1.0; 1.0 ]) );
    ]
  in
  let extract_points_tests =
    [
      ( "balloonpath extract_points empty list" >:: fun _ ->
        assert_equal [] (MTD.Balloonpath.extract_points []) );
    ]
  in

  let balloonpath_tests =
    List.flatten
      [
        check_turn_collide_tests @ turn_balloon_tests @ move_balloons_tests
        @ create_turn_point_test @ produce_point_tests @ extract_points_tests;
      ]
  in

  let value_of_balloon_tests =
    [
      ( "balloons value_of_balloons None" >:: fun _ ->
        assert_equal 0 (MTD.Balloons.value_of_balloon None) );
      ( "balloons value_of_balloons Red" >:: fun _ ->
        assert_equal 1 (MTD.Balloons.value_of_balloon Red) );
      ( "balloons value_of_balloons Blue" >:: fun _ ->
        assert_equal 2 (MTD.Balloons.value_of_balloon Blue) );
      ( "balloons value_of_balloons Green" >:: fun _ ->
        assert_equal 3 (MTD.Balloons.value_of_balloon Green) );
      ( "balloons value_of_balloons Orange" >:: fun _ ->
        assert_equal 4 (MTD.Balloons.value_of_balloon Yellow) );
      ( "balloons value_of_balloons Purple" >:: fun _ ->
        assert_equal 5 (MTD.Balloons.value_of_balloon Orange) );
      ( "balloons value_of_balloons Yellow" >:: fun _ ->
        assert_equal 6 (MTD.Balloons.value_of_balloon Purple) );
      ( "balloons value_of_balloons Lead" >:: fun _ ->
        assert_equal 7 (MTD.Balloons.value_of_balloon Lead) );
    ]
  in
  let balloon_of_value_tests = [] in
  let get_hitbox_tests = [] in
  let determine_image_tests = [] in
  let determine_velocity_tests = [] in
  let change_velocity_tests = [] in
  let make_balloon_tests = [] in
  let lower_lives_tests = [] in
  let remove_balloons_tests = [] in

  let balloons_tests =
    List.flatten
      [
        value_of_balloon_tests @ balloon_of_value_tests @ get_hitbox_tests
        @ determine_image_tests @ determine_velocity_tests
        @ change_velocity_tests @ make_balloon_tests @ lower_lives_tests
        @ remove_balloons_tests;
      ]
  in

  let tests = balloonpath_tests @ balloons_tests in
  let suite = "test suite for MTD" >::: List.flatten [ tests ] in

  (* FINISH THESE LATER *)
  (*
    let bears_tests = []
      let constants_tests = []
      let game_tests = []
      let gamebackground_tests = []
      let gamebounds_tests = []
      let menubar_tests = []
      let projectiles_tests = []
      let waves_tests = [] *)
  run_test_tt_main suite;

  Raylib.close_window ()

let () = run_tests ()
