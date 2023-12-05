open OUnit2
open MTD

(* MAKE BALLOON TO TEST *)

let draw_turnpoints_tests =
  [
    ( "balloonpath draw_turnpoints empty list" >:: fun _ ->
      assert_equal (Balloonpath.draw_turnpoints []) () );
    (* ( "balloonpath draw_turnpoints single element list" >:: fun _ ->
         assert_equal (MTD.Balloonpath.draw_turnpoints [ (10, 20, 3) ]) () );
       ( "balloonpath draw_turnpoints multi element list" >:: fun _ ->
         assert_equal (MTD.Balloonpath.draw_turnpoints [ (0, 2, 3); (2, 3, 1) ]) ()
       ); *)
  ]

let balloon1 = Balloons.make_balloon 1 Red false
let () = (Printf.printf "%b") balloon1.is_lead

let check_turn_collide_tests =
  [
    ( "balloonpath check_turn_collide empty list" >:: fun _ ->
      assert_equal (Balloonpath.check_turn_collide balloon1 []) None );
    (* ( "balloonpath check_turn_collide single element list" >:: fun _ ->
       assert_equal
         (MTD.Balloonpath.check_turn_collide balloon1 [ (10, 20, 0) ])
         (Some 1) ); *)
  ]

let turn_balloon_tests = []
let move_balloons_tests = []
let extract_points_tests = []
let point_json_parse_tests = []

let balloonpath_tests =
  List.flatten
    [
      draw_turnpoints_tests @ check_turn_collide_tests @ turn_balloon_tests
      @ move_balloons_tests @ extract_points_tests @ point_json_parse_tests;
    ]

(* FINISH THESE LATER *)
(* let balloons_tests = []
   let bears_tests = []
   let constants_tests = []
   let game_tests = []
   let gamebackground_tests = []
   let gamebounds_tests = []
   let menubar_tests = []
   let projectiles_tests = []
   let waves_tests = [] *)
let tests = balloonpath_tests @ []
let suite = "test suite for MTD" >::: List.flatten [ tests ]
let _ = run_test_tt_main suite
