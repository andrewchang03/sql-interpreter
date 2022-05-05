(** TEST PLAN: We tested mainly two components of our system: parsing
    the queries, and ensuring that the queries work on our database
    properly. Testing parsers is necessary to ensure that our queries
    are understood properly by our system. We omitted testing graphics
    because frankly, graphics cannot be tested with a certain output via
    OUnit. Instead, we manually tested it by seeing whether the gui
    displays the information we expect. Test cases were developed glass
    box mainly because we need to account for different incorrect parser
    inputs, but black box approach was mainly used for testing the
    correctness of query implementations. Essentially, Command module
    was tested glass box with OUnit. Table module was tested glassbox
    with OUnit to an extent, but it requires us to manually create
    expected csv output files and do other intermediary steps. Interface
    module was manaully tested. Overall, while it may not be perfect,
    this should demonstrate the correctness of our system as it should
    perform the expected operations when queries that satisfy the
    precondition well are provided. *)

open OUnit2
open Database
open Table
open Csv
open Command

(* format for testing regular functions *)
let test (test_name : string) expected_output actual_output : test =
  test_name >:: fun _ -> assert_equal expected_output actual_output

(* format for testing functions that return exceptions *)
let test_exn (test_name : string) (expected_output : exn) actual_output
    : test =
  test_name >:: fun _ -> assert_raises expected_output actual_output

let parse_tests =
  [
    test "parse Help" Help (parse "help");
    test "parse QueriesHelp" QueriesHelp (parse "queries help");
    test "parse Quit" Quit (parse "quit");
    test "parse ListTables" ListTables (parse "list");
    test "parse LoadTable" (LoadTable "table") (parse "load table");
    test "parse DisplayTable" (DisplayTable "table")
      (parse "display table");
    test "parse DROP TABLE" (DropTable "table")
      (parse "DROP TABLE table");
    test "parse SELECT one"
      (Select { table_name = "people"; col_names = [ "first_name" ] })
      (parse "SELECT first_name FROM people");
    test "parse SELECT multiple"
      (Select
         {
           table_name = "people";
           col_names = [ "first_name"; "last_name"; "birth_year" ];
         })
      (parse "SELECT first_name last_name birth_year FROM people");
    test "parse SELECT ALL" (SelectAll "table")
      (parse "SELECT ALL table");
    test "parse INSERT INTO"
      (InsertInto
         {
           table_name = "people";
           cols = [ ("name", STRING); ("age", INT); ("grade", STRING) ];
           vals = [ "henry"; "24"; "A+" ];
         })
      (parse
         "INSERT INTO people name_string age_int grade_string VALUES \
          henry 24 A+");
    test "parse CREATE TABLE"
      (CreateTable
         {
           table_name = "people";
           cols = [ ("name", STRING); ("age", INT); ("grade", STRING) ];
         })
      (parse "CREATE TABLE people name string age int grade string");
    test "parse UPDATE"
      (Update
         {
           table_name = "people";
           col_names = [ "first_name"; "last_name" ];
           vals = [ "katherine"; "heatzig" ];
           cond = { left = "office"; op = EQ; right = "gates" };
         })
      (parse
         "UPDATE people first_name last_name VALUES katherine heatzig \
          WHERE office = gates");
    test "parse UPDATE unsupported operator"
      (Update
         {
           table_name = "people";
           col_names = [ "first_name"; "last_name" ];
           vals = [ "katherine"; "heatzig" ];
           cond =
             { left = "office"; op = UNSUPPORTED "=="; right = "gates" };
         })
      (parse
         "UPDATE people first_name last_name VALUES katherine heatzig \
          WHERE office == gates");
    test "parse ALTER TABLE"
      (AlterTable
         {
           table_name = "people";
           alt_type = ADD;
           col_name = "age";
           col_type = INT;
         })
      (parse "ALTER TABLE people ADD age int");
    test "parse DELETE"
      (Delete
         {
           table_name = "table_name";
           cond = { left = "CustomerName"; op = EQ; right = "Albert" };
         })
      (parse "DELETE FROM table_name WHERE CustomerName = Albert");
  ]

let parse_exns =
  [
    test_exn "parse Empty" Empty (fun () -> parse " ");
    test_exn "unknown command" Malformed (fun () -> parse "view table");
    test_exn "wrong select all" Malformed (fun () -> parse "SELECT_ALL");
    test_exn "DROP TABLE missing table" Malformed (fun () ->
        parse "DROP TABLE");
    test_exn "parse SELECT Malformed, no table name" Malformed
      (fun () -> parse "SELECT column1 column2 FROM");
    test_exn "parse INSERT INTO, missing data types" Malformed
      (fun () ->
        parse "INSERT INTO table name age grade VALUES henry 24 A+");
    test_exn "parse INSERT INTO, no values" Malformed (fun () ->
        parse
          "INSERT INTO table name string age int grade string VALUES");
    test_exn "CREATE TABLE missing data types" Malformed (fun () ->
        parse "CREATE TABLE people name age grade");
    test_exn "DELETE TABLE missing table name" Malformed (fun () ->
        parse "DELETE FROM WHERE CustomerName = Albert");
    test_exn "DELETE TABLE no WHERE" Malformed (fun () ->
        parse "DELETE FROM table_name CustomerName = Albert");
    test_exn "DELETE TABLE no condition" Malformed (fun () ->
        parse "DELETE FROM table_name WHERE");
  ]

(* Table.ml tests *)

(** [update_test name table cols vals cond expected_output] constructs
    an OUnit test named [name] that asserts the quality of
    [expected_output] with [update name table cols vals cond]. *)
let create_test (name : string) (fname : string) cols expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (create_table fname cols)

let insert_cols =
  [ ("id", INT); ("student_name", STRING); ("grad_year", INT) ]

let insert_first = [ "0"; "dog"; "2025" ]

let insert_test (name : string) cols vals expected_output : test =
  name >:: fun _ ->
  let file = create_table "insert_test" insert_cols in
  let inserted = insert "insert_test" cols vals in
  assert_equal expected_output inserted;
  Sys.remove ("data" ^ Filename.dir_sep ^ "insert_test.csv")

let data_dir_prefix = "data" ^ Filename.dir_sep

let students_table =
  Csv.load ("data" ^ Filename.dir_sep ^ "students.csv")

let insert_compare =
  Csv.load ("data" ^ Filename.dir_sep ^ "insert_compare.csv")

(* Empty test suite template *)
let test_suite_1 = []

(* Table.ml test suite *)
let table_suite =
  [
    create_test "empty" "new" [ ("col1", INT) ] [ [ "col1 int" ] ];
    insert_test "insert into table" insert_cols insert_first
      insert_compare;
  ]

let suite =
  "Test suites"
  >::: List.flatten [ parse_tests; parse_exns; table_suite ]

let _ = run_test_tt_main suite
