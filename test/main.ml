open OUnit2
open Database
open Table
open Csv

let test (test_name : string) expected_output actual_output : test =
  test_name >:: fun _ -> assert_equal expected_output actual_output

(* Table.ml tests *)

(** [update_test name table cols vals cond expected_output] constructs
    an OUnit test named [name] that asserts the quality of
    [expected_output] with [update name table cols vals cond]. *)
let create_test (name : string) (fname : string) expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (create_table fname)

(** [update_test name table cols vals cond expected_output] constructs
    an OUnit test named [name] that asserts the quality of
    [expected_output] with [update name table cols vals cond]. *)
let update_test
    (name : string)
    (table : Csv.t)
    (cols : string list)
    (vals : string list)
    (cond : 'a -> 'b -> bool)
    (expected_output : Csv.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (update table cols vals cond)

let data_dir_prefix = "data" ^ Filename.dir_sep

let students_table =
  Csv.load ("data" ^ Filename.dir_sep ^ "students.csv")

(* Empty test suite template *)
let test_suite_1 = []

(* Table.ml test suite *)
let table_suite =
  [
    create_test "empty" "new" ();
    update_test "update students" students_table [ "" ] [ "" ] ( = ) [];
  ]

let suite = "Test suites" >::: List.flatten [ table_suite ]
let _ = run_test_tt_main suite
