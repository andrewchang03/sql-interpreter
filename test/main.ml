open OUnit2

let test (test_name : string) expected_output actual_output : test =
  test_name >:: fun _ -> assert_equal expected_output actual_output

(* Empty test suite template *)
let test_suite_1 = []
let suite = "Test suites" >::: List.flatten []
let _ = run_test_tt_main suite
