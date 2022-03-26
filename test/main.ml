open OUnit2
open Database
open Table
open Csv
open Command

(* format for testing regular functions *)
let test (test_name : string) expected_output actual_output : test =
  test_name >:: fun _ -> assert_equal expected_output actual_output

(* format for testing functions that return exceptions *)
let test_exn (test_name : string) expected_output actual_output : test =
  test_name >:: fun _ -> assert_raises expected_output actual_output

let parse_tests =
  [
    test_exn "parse Empty" Empty (fun () -> parse " ");
    test "parse Help" Help (parse "help");
    test "parse Quit" Quit (parse "quit");
    test "parse ListTables" ListTables (parse "list");
    test "parse LoadTable" (LoadTable "table") (parse "load table");
    test "parse DisplayTable" (DisplayTable "table")
      (parse "display table");
    test "parse DROP TABLE" (DropTable "table")
      (parse "DROP TABLE table");
    test "parse SELECT"
      (Select { table_name = "people"; col_names = [ "city"; "bruh" ] })
      (parse "SELECT city bruh FROM people");
    test_exn "parse SELECT Malformed" Malformed (fun () ->
        parse "SELECT a b FROM");
    test "parse INSERT INTO"
      (InsertInto
         {
           table_name = "table";
           col_names = [ "a"; "b" ];
           vals = [ "1"; "2" ];
         })
      (parse "INSERT INTO table a b VALUES 1 2");
  ]

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
    create_test "empty" "new" ()
    (* update_test "update students" students_table [ "" ] [ "" ] ( = )
       []; *);
  ]

let suite = "Test suites" >::: List.flatten [ parse_tests; table_suite ]
let _ = run_test_tt_main suite
