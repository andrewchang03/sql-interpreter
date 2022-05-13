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
open Parse

(* format for testing regular functions *)
let test
    (test_name : string)
    (expected_output : 'a)
    (actual_output : 'a) : test =
  test_name >:: fun _ -> assert_equal expected_output actual_output

(* format for testing functions that return exceptions *)
let test_exn
    (test_name : string)
    (expected_output : exn)
    (actual_output : unit -> 'a) : test =
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
      (Select { table_name = "people"; cols = [ "first_name" ] })
      (parse "SELECT first_name FROM people");
    test "parse SELECT multiple"
      (Select
         {
           table_name = "people";
           cols =
             [
               "first_name:string";
               "last_name:string";
               "birth_year:int";
               "birth_month:int";
             ];
         })
      (parse
         "SELECT first_name:string last_name:string birth_year:int \
          birth_month:int FROM people");
    test "parse SELECT FROM WHERE"
      (SelectWhere
         {
           table_name = "students";
           cond = { left = "grade"; op = EQ; right = "A" };
         })
      (parse "SELECT FROM students WHERE grade = A");
    test "parse SELECT ALL" (SelectAll "table")
      (parse "SELECT ALL table");
    test "parse INSERT INTO"
      (InsertInto
         {
           table_name = "people";
           cols =
             [
               "first_name:string";
               "last_name:string";
               "age:int";
               "grade:string";
             ];
           vals = [ "henry"; "williams"; "24"; "A+" ];
         })
      (parse
         "INSERT INTO people first_name:string last_name:string \
          age:int grade:string VALUES henry williams 24 A+");
    test "parse CREATE TABLE"
      (CreateTable
         {
           table_name = "students";
           cols =
             [
               ("first_name", STRING);
               ("last_name", STRING);
               ("age", INT);
               ("grade", STRING);
             ];
         })
      (parse
         "CREATE TABLE students first_name string last_name string age \
          int grade string");
    test "parse UPDATE"
      (Update
         {
           table_name = "people";
           cols =
             [
               "first_name:string";
               "last_name:string";
               "birth_year:int";
               "birth_month:int";
             ];
           vals = [ "katherine"; "heatzig"; "2001"; "12" ];
           cond = { left = "office"; op = EQ; right = "gates" };
         })
      (parse
         "UPDATE people first_name:string last_name:string \
          birth_year:int birth_month:int VALUES katherine heatzig 2001 \
          12 WHERE office = gates");
    test "parse UPDATE unsupported operator"
      (Update
         {
           table_name = "people";
           cols = [ "first_name:string"; "last_name:string" ];
           vals = [ "katherine"; "heatzig" ];
           cond =
             { left = "office"; op = UNSUPPORTED "=="; right = "gates" };
         })
      (parse
         "UPDATE people first_name:string last_name:string VALUES \
          katherine heatzig WHERE office == gates");
    test "parse ALTER TABLE ADD"
      (AlterTable
         {
           table_name = "people";
           alt_type = ADD;
           col_name = "age";
           col_type = INT;
         })
      (parse "ALTER TABLE people ADD age int");
    test "parse ALTER TABLE UNSUPPORTED"
      (AlterTable
         {
           table_name = "processor";
           alt_type = UNSUPPORTED "ADDX";
           col_name = "manufacturer";
           col_type = STRING;
         })
      (parse "ALTER TABLE processor ADDX manufacturer string");
    test "parse DELETE"
      (Delete
         {
           table_name = "table_name";
           cond =
             {
               left = "customer_full_name";
               op = EQ;
               right = "Alexander_Hamilton";
             };
         })
      (parse
         "DELETE FROM table_name WHERE customer_full_name = \
          Alexander_Hamilton");
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
    test_exn "parse INSERT INTO, no values" Malformed (fun () ->
        parse "INSERT INTO table name age grade VALUES");
    test_exn "DELETE TABLE missing table name" Malformed (fun () ->
        parse "DELETE FROM WHERE CustomerName = Albert");
    test_exn "DELETE TABLE no WHERE" Malformed (fun () ->
        parse "DELETE FROM table_name CustomerName = Albert");
    test_exn "DELETE TABLE no condition" Malformed (fun () ->
        parse "DELETE FROM table_name WHERE");
    test_exn "ALTER TABLE no data type" Malformed (fun () ->
        parse "ALTER TABLE people ADD age");
    test_exn "ALTER TABLE no table name" Malformed (fun () ->
        parse "ALTER TABLE ADD age int");
    test_exn "ALTER TABLE no operation specification ADD" Malformed
      (fun () -> parse "ALTER TABLE table_name age int");
  ]

(* Table operations tests *)
let query_tests =
  [
    test "CREATE TABLE file creation with columns"
      (let _ =
         Csv.save
           ("data" ^ Filename.dir_sep ^ "create_compare" ^ ".csv")
           [
             [
               "first_name:string";
               "last_name:string";
               "age:int";
               "grade:string";
             ];
           ]
       in
       Csv.load ("data" ^ Filename.dir_sep ^ "create_compare" ^ ".csv"))
      (create_table "create"
         [
           ("first_name", STRING);
           ("last_name", STRING);
           ("age", INT);
           ("grade", STRING);
         ]);
    test "SELECT columns"
      [
        [ "name:string"; "age:int" ];
        [ "michael"; "15" ];
        [ "robert"; "30" ];
        [ "cornell"; "100" ];
        [ "mesut"; "20" ];
      ]
      (let table =
         Csv.load ("data" ^ Filename.dir_sep ^ "sample" ^ ".csv")
       in
       select table [ "name:string"; "age:int" ]);
    test "INSERT INTO one"
      (Csv.load ("data" ^ Filename.dir_sep ^ "create_compare" ^ ".csv")
      @ [ [ "jenna"; "parker"; "19"; "B+" ] ])
      (insert "create"
         [
           "first_name:string";
           "last_name:string";
           "age:int";
           "grade:string";
         ]
         [ "jenna"; "parker"; "19"; "B+" ]);
  ]

let suite =
  "Test suites"
  >::: List.flatten [ parse_tests; parse_exns; query_tests ]

let _ = run_test_tt_main suite
