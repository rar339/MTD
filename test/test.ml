open OUnit2
open MTD

let vel0 = Raylib.Vector2.create 0.0 0.0
let vel1x = Raylib.Vector2.create 1.0 0.0
let vel1y = Raylib.Vector2.create 0.0 1.0
let vel1negx = Raylib.Vector2.create (-1.0) 0.0
let vel1negy = Raylib.Vector2.create 0.0 (-1.0)

(* MAKE BALLOON TO TEST *)
let make_test_balloon x_pos y_pos current_turn velocity color : Balloons.balloon
    =
  {
    color;
    velocity;
    position = Raylib.Vector2.create x_pos y_pos;
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
  let balloon1 = make_test_balloon 0.0 0.0 0 vel0 Lead in
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
    let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
    [
      (let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 1.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 1)
       in
       "balloonpath turn_balloon multi element list, yes turn 1" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create (-1.0) 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 2)
       in
       "balloonpath turn_balloon multi element list, yes turn 2" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 (-1.0) in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 3)
       in
       "balloonpath turn_balloon multi element list, yes turn 3" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 1.0 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 4)
       in
       "balloonpath turn_balloon multi element list, yes turn 4" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 (-1.0) in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 5)
       in
       "balloonpath turn_balloon multi element list, yes turn 5" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 1.0 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 6)
       in
       "balloonpath turn_balloon multi element list, yes turn 6" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 1.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 7)
       in
       "balloonpath turn_balloon multi element list, yes turn 7" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create (-1.0) 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 8)
       in
       "balloonpath turn_balloon multi element list, yes turn 8" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 0.0 1.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 9)
       in
       "balloonpath turn_balloon multi element list, yes turn 9" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
       let (v1 : Raylib.Vector2.t) = Raylib.Vector2.create 1.0 0.0 in
       let (v2 : Raylib.Vector2.t) =
         MTD.Balloonpath.turn_balloon balloon 1.0 (0, 0, 10)
       in
       "balloonpath turn_balloon multi element list, yes turn 10" >:: fun _ ->
       assert_equal
         (Raylib.Vector2.x v1, Raylib.Vector2.y v1)
         (Raylib.Vector2.x v2, Raylib.Vector2.y v2));
      (let balloon = make_test_balloon 8.0 8.0 1 vel0 Lead in
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
           make_test_balloon 1.0 1.0 1 vel0 Lead;
           make_test_balloon 1.0 1.0 1 vel0 Red;
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
           make_test_balloon 1.0 1.0 1 vel0 Lead;
           make_test_balloon 1.0 1.0 1 vel0 Red;
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
  let balloon_of_value_tests =
    [
      ( "balloons balloon_of_value 0" >:: fun _ ->
        assert_equal MTD.Balloons.None (MTD.Balloons.balloon_of_value 0) );
      ( "balloons balloon_of_value 1" >:: fun _ ->
        assert_equal MTD.Balloons.Red (MTD.Balloons.balloon_of_value 1) );
      ( "balloons balloon_of_value 2" >:: fun _ ->
        assert_equal MTD.Balloons.Blue (MTD.Balloons.balloon_of_value 2) );
      ( "balloons balloon_of_value 3" >:: fun _ ->
        assert_equal MTD.Balloons.Green (MTD.Balloons.balloon_of_value 3) );
      ( "balloons balloon_of_value 4" >:: fun _ ->
        assert_equal MTD.Balloons.Yellow (MTD.Balloons.balloon_of_value 4) );
      ( "balloons balloon_of_value 5" >:: fun _ ->
        assert_equal MTD.Balloons.Orange (MTD.Balloons.balloon_of_value 5) );
      ( "balloons balloon_of_value 6" >:: fun _ ->
        assert_equal MTD.Balloons.Purple (MTD.Balloons.balloon_of_value 6) );
      ( "balloons balloon_of_value 7" >:: fun _ ->
        assert_equal MTD.Balloons.Lead (MTD.Balloons.balloon_of_value 7) );
    ]
  in
  let get_hitbox_tests =
    [
      (let balloon = make_test_balloon 0.0 0.0 0 vel0 Lead in
       let rect = MTD.Balloons.get_hitbox balloon in
       "balloons get_hitbox starting balloon" >:: fun _ ->
       assert_equal (0.0, 0.0, 0.0, 0.0)
         ( Raylib.Rectangle.x rect,
           Raylib.Rectangle.y rect,
           Raylib.Rectangle.height rect,
           Raylib.Rectangle.width rect ));
      (let balloon = make_test_balloon 1.0 1.0 0 vel0 Lead in
       let rect = MTD.Balloons.get_hitbox balloon in
       "balloons get_hitbox moving balloon" >:: fun _ ->
       assert_equal (1.0, 1.0, 0.0, 0.0)
         ( Raylib.Rectangle.x rect,
           Raylib.Rectangle.y rect,
           Raylib.Rectangle.height rect,
           Raylib.Rectangle.width rect ));
    ]
  in
  let determine_image_tests =
    [
      (Constants.red_balloon_img :=
         Some (Raylib.load_texture "./img/balloons/red.png");
       "balloons determine_image Red" >:: fun _ ->
       assert_equal
         (Option.get !Constants.red_balloon_img)
         (MTD.Balloons.determine_image MTD.Balloons.Red));
      (Constants.blue_balloon_img :=
         Some (Raylib.load_texture "./img/balloons/blue.png");
       "balloons determine_image Blue" >:: fun _ ->
       assert_equal
         (Option.get !Constants.blue_balloon_img)
         (MTD.Balloons.determine_image MTD.Balloons.Blue));
      (Constants.green_balloon_img :=
         Some (Raylib.load_texture "./img/balloons/green.png");
       "balloons determine_image Green" >:: fun _ ->
       assert_equal
         (Option.get !Constants.green_balloon_img)
         (MTD.Balloons.determine_image MTD.Balloons.Green));
      (Constants.yellow_balloon_img :=
         Some (Raylib.load_texture "./img/balloons/yellow.png");
       "balloons determine_image Yellow" >:: fun _ ->
       assert_equal
         (Option.get !Constants.yellow_balloon_img)
         (MTD.Balloons.determine_image MTD.Balloons.Yellow));
      (Constants.orange_balloon_img :=
         Some (Raylib.load_texture "./img/balloons/orange.png");
       "balloons determine_image Yellow" >:: fun _ ->
       assert_equal
         (Option.get !Constants.orange_balloon_img)
         (MTD.Balloons.determine_image MTD.Balloons.Orange));
      (Constants.purple_balloon_img :=
         Some (Raylib.load_texture "./img/balloons/purple.png");
       "balloons determine_image Purple" >:: fun _ ->
       assert_equal
         (Option.get !Constants.purple_balloon_img)
         (MTD.Balloons.determine_image MTD.Balloons.Purple));
      (Constants.lead_balloon_img :=
         Some (Raylib.load_texture "./img/balloons/lead.png");
       "balloons determine_image Lead" >:: fun _ ->
       assert_equal
         (Option.get !Constants.lead_balloon_img)
         (MTD.Balloons.determine_image MTD.Balloons.Lead));
    ]
  in
  let determine_velocity_tests =
    [
      ( "balloons determine_velocity None" >:: fun _ ->
        assert_equal 0.0 (MTD.Balloons.determine_velocity MTD.Balloons.None) );
      ( "balloons determine_velocity Red" >:: fun _ ->
        assert_equal 2.0 (MTD.Balloons.determine_velocity MTD.Balloons.Red) );
      ( "balloons determine_velocity Blue" >:: fun _ ->
        assert_equal 3.0 (MTD.Balloons.determine_velocity MTD.Balloons.Blue) );
      ( "balloons determine_velocity Green" >:: fun _ ->
        assert_equal 4.0 (MTD.Balloons.determine_velocity MTD.Balloons.Green) );
      ( "balloons determine_velocity Yellow" >:: fun _ ->
        assert_equal 4.5 (MTD.Balloons.determine_velocity MTD.Balloons.Yellow)
      );
      ( "balloons determine_velocity Orange" >:: fun _ ->
        assert_equal 4.5 (MTD.Balloons.determine_velocity MTD.Balloons.Orange)
      );
      ( "balloons determine_velocity Purple" >:: fun _ ->
        assert_equal 5.5 (MTD.Balloons.determine_velocity MTD.Balloons.Purple)
      );
      ( "balloons determine_velocity Lead" >:: fun _ ->
        assert_equal 3.0 (MTD.Balloons.determine_velocity MTD.Balloons.Lead) );
    ]
  in
  let change_velocity_tests =
    [
      (let balloon = make_test_balloon 0.0 0.0 0 vel1x Blue in
       "balloons change_velocity Blue to Red pos x" >:: fun _ ->
       let new_vel = MTD.Balloons.change_velocity balloon MTD.Balloons.Red in
       assert_equal (2.0, 0.0)
         (Raylib.Vector2.x new_vel, Raylib.Vector2.y new_vel));
      (let balloon = make_test_balloon 0.0 0.0 0 vel1y Blue in
       "balloons change_velocity Blue to Red pos y" >:: fun _ ->
       let new_vel = MTD.Balloons.change_velocity balloon MTD.Balloons.Red in
       assert_equal (0.0, 2.0)
         (Raylib.Vector2.x new_vel, Raylib.Vector2.y new_vel));
      (let balloon = make_test_balloon 0.0 0.0 0 vel1negx Blue in
       "balloons change_velocity Blue to Red neg x" >:: fun _ ->
       let new_vel = MTD.Balloons.change_velocity balloon MTD.Balloons.Red in
       assert_equal (-2.0, 0.0)
         (Raylib.Vector2.x new_vel, Raylib.Vector2.y new_vel));
      (let balloon = make_test_balloon 0.0 0.0 0 vel1negy Blue in
       "balloons change_velocity Blue to Red neg y" >:: fun _ ->
       let new_vel = MTD.Balloons.change_velocity balloon MTD.Balloons.Red in
       assert_equal (0.0, -2.0)
         (Raylib.Vector2.x new_vel, Raylib.Vector2.y new_vel));
    ]
  in
  let make_balloon_tests =
    [
      (let balloon = MTD.Balloons.make_balloon Red in
       "balloons make_balloon Red" >:: fun _ ->
       assert_equal
         (-30.0, 87.0, 2.0, 0.0, MTD.Balloons.Red)
         ( Raylib.Vector2.x balloon.position,
           Raylib.Vector2.y balloon.position,
           Raylib.Vector2.x balloon.velocity,
           Raylib.Vector2.y balloon.velocity,
           balloon.color ));
      (let balloon = MTD.Balloons.make_balloon Blue in
       "balloons make_balloon Blue" >:: fun _ ->
       assert_equal
         (-30.0, 87.0, 3.0, 0.0, MTD.Balloons.Blue)
         ( Raylib.Vector2.x balloon.position,
           Raylib.Vector2.y balloon.position,
           Raylib.Vector2.x balloon.velocity,
           Raylib.Vector2.y balloon.velocity,
           balloon.color ));
      (let balloon = MTD.Balloons.make_balloon Green in
       "balloons make_balloon Green" >:: fun _ ->
       assert_equal
         (-30.0, 87.0, 4.0, 0.0, MTD.Balloons.Green)
         ( Raylib.Vector2.x balloon.position,
           Raylib.Vector2.y balloon.position,
           Raylib.Vector2.x balloon.velocity,
           Raylib.Vector2.y balloon.velocity,
           balloon.color ));
      (let balloon = MTD.Balloons.make_balloon Yellow in
       "balloons make_balloon Yellow" >:: fun _ ->
       assert_equal
         (-30.0, 87.0, 4.5, 0.0, MTD.Balloons.Yellow)
         ( Raylib.Vector2.x balloon.position,
           Raylib.Vector2.y balloon.position,
           Raylib.Vector2.x balloon.velocity,
           Raylib.Vector2.y balloon.velocity,
           balloon.color ));
      (let balloon = MTD.Balloons.make_balloon Orange in
       "balloons make_balloon Orange" >:: fun _ ->
       assert_equal
         (-30.0, 87.0, 4.5, 0.0, MTD.Balloons.Orange)
         ( Raylib.Vector2.x balloon.position,
           Raylib.Vector2.y balloon.position,
           Raylib.Vector2.x balloon.velocity,
           Raylib.Vector2.y balloon.velocity,
           balloon.color ));
      (let balloon = MTD.Balloons.make_balloon Purple in
       "balloons make_balloon Purple" >:: fun _ ->
       assert_equal
         (-30.0, 87.0, 5.5, 0.0, MTD.Balloons.Purple)
         ( Raylib.Vector2.x balloon.position,
           Raylib.Vector2.y balloon.position,
           Raylib.Vector2.x balloon.velocity,
           Raylib.Vector2.y balloon.velocity,
           balloon.color ));
      (let balloon = MTD.Balloons.make_balloon Lead in
       "balloons make_balloon Lead" >:: fun _ ->
       assert_equal
         (-30.0, 87.0, 3.0, 0.0, MTD.Balloons.Lead)
         ( Raylib.Vector2.x balloon.position,
           Raylib.Vector2.y balloon.position,
           Raylib.Vector2.x balloon.velocity,
           Raylib.Vector2.y balloon.velocity,
           balloon.color ));
    ]
  in
  let remove_balloons_tests =
    [
      ( "balloons remove_balloons empty list" >:: fun _ ->
        assert_equal [] (MTD.Balloons.remove_balloons []) );
      (let balloon = make_test_balloon 0.0 0.0 0 vel0 MTD.Balloons.Red in
       "balloons remove_balloons don't remove balloon" >:: fun _ ->
       assert_equal (balloon :: [])
         (MTD.Balloons.remove_balloons (balloon :: [])));
      (let balloon = make_test_balloon 0.0 0.0 0 vel0 MTD.Balloons.Red in
       balloon.remove <- true;
       "balloons remove_balloons remove balloon" >:: fun _ ->
       assert_equal [] (MTD.Balloons.remove_balloons (balloon :: [])));
      (let balloon = make_test_balloon 0.0 (-1.0) 0 vel0 MTD.Balloons.Red in
       "balloons remove_balloons remove balloon" >:: fun _ ->
       assert_equal [] (MTD.Balloons.remove_balloons (balloon :: [])));
    ]
  in

  let balloons_tests =
    List.flatten
      [
        value_of_balloon_tests @ balloon_of_value_tests @ get_hitbox_tests
        @ determine_image_tests @ determine_velocity_tests
        @ change_velocity_tests @ make_balloon_tests @ remove_balloons_tests;
      ]
  in

  let determine_image_tests =
    [
      (Constants.menu_dartbear_img :=
         Some (Raylib.load_texture "./img/bears/menu_dartbear.png");
       "bears determine_image Menu Dart Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.menu_dartbear_img)
         (MTD.Bears.determine_image true MTD.Bears.Dart));
      (Constants.dartbear_img :=
         Some (Raylib.load_texture "./img/bears/dartbear.png");
       "bears determine_image Dart Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.dartbear_img)
         (MTD.Bears.determine_image false MTD.Bears.Dart));
      (Constants.menu_hockeybear_img :=
         Some (Raylib.load_texture "./img/bears/menu_hockeybear.png");
       "bears determine_image Menu Hockey Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.menu_hockeybear_img)
         (MTD.Bears.determine_image true MTD.Bears.Hockey));
      (Constants.hockeybear_img :=
         Some (Raylib.load_texture "./img/bears/hockeybear.png");
       "bears determine_image Hockey Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.hockeybear_img)
         (MTD.Bears.determine_image false MTD.Bears.Hockey));
      (Constants.menu_polarbear_img :=
         Some (Raylib.load_texture "./img/bears/menu_zombiebear.png");
       "bears determine_image Menu Polar Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.menu_polarbear_img)
         (MTD.Bears.determine_image true MTD.Bears.Polar));
      (Constants.polarbear_img :=
         Some (Raylib.load_texture "./img/bears/zombiebear.png");
       "bears determine_image Polar Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.polarbear_img)
         (MTD.Bears.determine_image false MTD.Bears.Polar));
      (Constants.menu_sniperbear_img :=
         Some (Raylib.load_texture "./img/bears/menu_sniperbear.png");
       "bears determine_image Menu Sniper Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.menu_sniperbear_img)
         (MTD.Bears.determine_image true MTD.Bears.Sniper));
      (Constants.sniperbear_img :=
         Some (Raylib.load_texture "./img/bears/sniperbear.png");
       "bears determine_image Sniper Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.sniperbear_img)
         (MTD.Bears.determine_image false MTD.Bears.Sniper));
      (Constants.menu_dragonbear_img :=
         Some (Raylib.load_texture "./img/bears/redbear.png");
       "bears determine_image Menu Dragon Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.menu_dragonbear_img)
         (MTD.Bears.determine_image true MTD.Bears.Dragon));
      (Constants.dragonbear_img :=
         Some (Raylib.load_texture "./img/bears/redbear.png");
       "bears determine_image Dart Bear" >:: fun _ ->
       assert_equal
         (Option.get !Constants.dragonbear_img)
         (MTD.Bears.determine_image false MTD.Bears.Dragon));
    ]
  in
  let string_of_beartype_tests =
    [
      ( "bears string_of_beartype Dart Bear" >:: fun _ ->
        assert_equal "Dart" (MTD.Bears.string_of_beartype MTD.Bears.Dart) );
      ( "bears string_of_beartype Hockey Bear" >:: fun _ ->
        assert_equal "Hockey" (MTD.Bears.string_of_beartype MTD.Bears.Hockey) );
      ( "bears string_of_beartype Dart Polar" >:: fun _ ->
        assert_equal "Polar" (MTD.Bears.string_of_beartype MTD.Bears.Polar) );
      ( "bears string_of_beartype Sniper Bear" >:: fun _ ->
        assert_equal "Sniper" (MTD.Bears.string_of_beartype MTD.Bears.Sniper) );
      ( "bears string_of_beartype Dragon Bear" >:: fun _ ->
        assert_equal "Dragon" (MTD.Bears.string_of_beartype MTD.Bears.Dragon) );
    ]
  in
  let generate_menu_bears_tests =
    [
      ( "bears generate_menu_bears" >:: fun _ ->
        assert_equal 5 (List.length (MTD.Bears.generate_menu_bears 0.0 0.0)) );
    ]
  in
  let determine_bear_clicked_tests =
    [
      ( "bears determine_bear_clicked empty list" >:: fun _ ->
        assert_equal None
          (MTD.Bears.determine_bear_clicked (Raylib.Vector2.create 0.0 0.0) [])
      );
      ( "bears determine_bear_clicked multi element list no hit" >:: fun _ ->
        assert_equal None
          (MTD.Bears.determine_bear_clicked
             (Raylib.Vector2.create 0.0 0.0)
             (MTD.Bears.generate_menu_bears 100.0 100.0)) );
      (let bear =
         MTD.Bears.determine_bear_clicked
           (Raylib.Vector2.create 0.0 0.0)
           (MTD.Bears.generate_menu_bears 0.0 0.0)
       in
       "bears determine_bear_clicked multi element list hit" >:: fun _ ->
       assert_equal MTD.Bears.Dart (Option.get bear).bear_type);
    ]
  in
  let check_collision_bears_tests =
    [
      ( "bears check_collision_bears yes " >:: fun _ ->
        assert_equal true
          (MTD.Bears.check_collision_bears
             (Some
                (MTD.Bears.make_bear false MTD.Bears.Dart
                   (Raylib.Vector2.create 0.0 0.0)))
             (MTD.Bears.generate_menu_bears 0.0 0.0)) );
      ( "bears check_collision_bears no " >:: fun _ ->
        assert_equal false
          (MTD.Bears.check_collision_bears
             (Some
                (MTD.Bears.make_bear false MTD.Bears.Dart
                   (Raylib.Vector2.create 0.0 0.0)))
             (MTD.Bears.generate_menu_bears 100.0 100.0)) );
    ]
  in
  let remove_bears_tests =
    [
      ( "bears remove_bears empty list " >:: fun _ ->
        assert_equal [] (MTD.Bears.remove_bears []) );
      (let bear =
         MTD.Bears.make_bear false MTD.Bears.Dart
           (Raylib.Vector2.create 0.0 0.0)
       in
       bear.sold <- true;
       "bears remove_bears sold bear " >:: fun _ ->
       assert_equal [] (MTD.Bears.remove_bears (bear :: [])));
      (let bear =
         MTD.Bears.make_bear false MTD.Bears.Dart
           (Raylib.Vector2.create 0.0 0.0)
       in
       "bears remove_bears remove none " >:: fun _ ->
       assert_equal (bear :: []) (MTD.Bears.remove_bears (bear :: [])));
    ]
  in

  let bears_tests =
    List.flatten
      [
        determine_image_tests @ string_of_beartype_tests
        @ generate_menu_bears_tests @ determine_bear_clicked_tests
        @ check_collision_bears_tests @ remove_bears_tests;
      ]
  in

  let tests = balloonpath_tests @ balloons_tests @ bears_tests in
  let suite = "test suite for MTD" >::: List.flatten [ tests ] in

  (* FINISH THESE LATER *)
  (*
      let game_tests = []
      let gamebackground_tests = []
      let gamebounds_tests = []
      let menubar_tests = []
      let projectiles_tests = []
      let waves_tests = [] *)
  run_test_tt_main suite;

  Raylib.close_window ()

let () = run_tests ()
