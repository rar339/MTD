open OUnit2
(*open MTD*)

let tests = [ ("EXAMPLE" >:: fun _ -> assert_equal 5 5) ]
let suite = "test suite for MTD" >::: List.flatten [ tests ]
let _ = run_test_tt_main suite
