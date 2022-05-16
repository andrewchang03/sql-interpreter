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
    module was manually tested. Overall, while it may not be perfect,
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
let test_exceptions
    (test_name : string)
    (expected_output : exn)
    (actual_output : unit -> 'a) : test =
  test_name >:: fun _ -> assert_raises expected_output actual_output

let remove_first = function
  | [] -> []
  | h :: t -> t

let rec apply_list func name cols = function
  | [] -> []
  | h :: t -> func name cols h :: apply_list func name cols t

let copy_file test_name file_name cols =
  if Sys.file_exists ("data" ^ Filename.dir_sep ^ file_name ^ ".csv")
  then
    let _ = create_table test_name cols in
    let _ =
      apply_list insert test_name cols
        (List.rev
           (remove_first
              (Csv.load
                 ("data" ^ Filename.dir_sep ^ file_name ^ ".csv"))))
    in
    ()
  else
    let _ = create_table test_name cols in
    ()

let insert_test
    (name : string)
    (insert_file : string)
    cols
    (vals : string list list)
    expected_output : test =
  name >:: fun _ ->
  let test_name = name ^ "_test" in
  copy_file test_name insert_file cols;
  let _ = apply_list insert test_name cols (List.rev vals) in
  let compare =
    Csv.compare
      (Csv.load ("data" ^ Filename.dir_sep ^ test_name ^ ".csv"))
      (Csv.load ("data" ^ Filename.dir_sep ^ expected_output))
  in
  assert_equal compare 0;
  Sys.remove ("data" ^ Filename.dir_sep ^ name ^ "_test.csv")

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
           col_names =
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
           cond = { left = "grade:int"; op = EQ; right = "A" };
         })
      (parse "SELECT FROM students WHERE grade:int = A");
    test "parse SELECT ALL" (SelectAll "table")
      (parse "SELECT ALL table");
    test "parse INSERT INTO"
      (InsertInto
         {
           table_name = "people";
           cols =
             [
               ("first_name", STRING);
               ("last_name", STRING);
               ("age", INT);
               ("grade", STRING);
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

let parse_exceptions =
  [
    test_exceptions "parse Empty" Empty (fun () -> parse " ");
    test_exceptions "unknown command" Malformed (fun () ->
        parse "view table");
    test_exceptions "wrong select all" Malformed (fun () ->
        parse "SELECT_ALL");
    test_exceptions "DROP TABLE missing table" Malformed (fun () ->
        parse "DROP TABLE");
    test_exceptions "parse SELECT Malformed, no table name" Malformed
      (fun () -> parse "SELECT column1 column2 FROM");
    test_exceptions "parse INSERT INTO, no values" Malformed (fun () ->
        parse "INSERT INTO table name age grade VALUES");
    test_exceptions "DELETE TABLE missing table name" Malformed
      (fun () -> parse "DELETE FROM WHERE CustomerName = Albert");
    test_exceptions "DELETE TABLE no WHERE" Malformed (fun () ->
        parse "DELETE FROM table_name CustomerName = Albert");
    test_exceptions "DELETE TABLE no condition" Malformed (fun () ->
        parse "DELETE FROM table_name WHERE");
    test_exceptions "ALTER TABLE no data type" Malformed (fun () ->
        parse "ALTER TABLE people ADD age");
    test_exceptions "ALTER TABLE no table name" Malformed (fun () ->
        parse "ALTER TABLE ADD age int");
    test_exceptions "ALTER TABLE no operation specification ADD"
      Malformed (fun () -> parse "ALTER TABLE table_name age int");
    test_exceptions "SELECT from empty table, returns failure"
      Database.Parse.NoTable (fun () -> select [] []);
    test_exceptions "AGGREGATE STRING FROM" Malformed (fun () ->
        parse "AGGREGATE STRING FROM sample age:int average");
    test_exceptions "AGGREGATE FORGOT FROM" Malformed (fun () ->
        parse "AGGREGATE INT sample age:int average");
    test_exceptions "AGGREGATE FORGOT operation type" Malformed
      (fun () -> parse "AGGREGATE INT sample age:int");
    test_exceptions "CREATE TABLE wrong data type" Malformed (fun () ->
        create_table "create2"
          [
            ("first_name", STRING);
            ("last_name", STRING);
            ("age", INT);
            ("grade", UNSUPPORTED "");
          ]);
    test_exceptions "parse CREATE TABLE empty" Malformed (fun () ->
        parse "CREATE TABLE empty");
    test_exceptions "ALTER TABLE wrong data type" Malformed (fun () ->
        alter_table_add
          [
            ( "alter_copy",
              Csv.load ("data" ^ Filename.dir_sep ^ "sample" ^ ".csv")
            );
          ]
          "alter_copy" "new" (UNSUPPORTED ""));
  ]

(* Table operations tests *)
let create_drop_tests =
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
    test "DROP TABLE"
      [ ("table1", [ [ "col1" ] ]); ("table3", [ [ "col1" ] ]) ]
      (drop_table
         [
           ("table1", [ [ "col1" ] ]);
           ("table2", [ [ "col1" ] ]);
           ("table3", [ [ "col1" ] ]);
         ]
         "table2");
    test "DROP TABLE from empty" [] (drop_table [] "table_name");
  ]

let select_insert_tests =
  [
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
    test "SELECT FROM WHERE equal"
      (Csv.load
         ("data" ^ Filename.dir_sep ^ "select_where_equal" ^ ".csv"))
      (let _ =
         Csv.save
           ("data" ^ Filename.dir_sep ^ "delete_copy" ^ ".csv")
           (Csv.load ("data" ^ Filename.dir_sep ^ "delete" ^ ".csv"))
       in
       select_where_table
         (Csv.load
            ("data" ^ Filename.dir_sep ^ "select_where" ^ ".csv"))
         "name:string" EQ "cornell");
    test "SELECT FROM WHERE greater than equal"
      (Csv.load
         ("data" ^ Filename.dir_sep ^ "select_where_greater" ^ ".csv"))
      (let _ =
         Csv.save
           ("data" ^ Filename.dir_sep ^ "delete_copy" ^ ".csv")
           (Csv.load ("data" ^ Filename.dir_sep ^ "delete" ^ ".csv"))
       in
       select_where_table
         (Csv.load
            ("data" ^ Filename.dir_sep ^ "select_where" ^ ".csv"))
         "name:string" GE "cornell");
    test "SELECT FROM WHERE less"
      (Csv.load
         ("data" ^ Filename.dir_sep ^ "select_where_less" ^ ".csv"))
      (let _ =
         Csv.save
           ("data" ^ Filename.dir_sep ^ "delete_copy" ^ ".csv")
           (Csv.load ("data" ^ Filename.dir_sep ^ "delete" ^ ".csv"))
       in
       select_where_table
         (Csv.load
            ("data" ^ Filename.dir_sep ^ "select_where" ^ ".csv"))
         "name:string" LESS "michael");
    test "SELECT ALL from a table"
      (Csv.load ("data" ^ Filename.dir_sep ^ "sample" ^ ".csv"))
      (select_all
         [
           ( "sample",
             Csv.load ("data" ^ Filename.dir_sep ^ "sample" ^ ".csv") );
           ( "sample2",
             Csv.load ("data" ^ Filename.dir_sep ^ "delete" ^ ".csv") );
           ("sample3", []);
         ]
         "sample");
    test "INSERT INTO once"
      (Csv.load ("data" ^ Filename.dir_sep ^ "create_compare" ^ ".csv")
      @ [ [ "jenna"; "parker"; "19"; "B+" ] ])
      (insert "create"
         [
           ("first_name", STRING);
           ("last_name", STRING);
           ("age", INT);
           ("grade", STRING);
         ]
         [ "jenna"; "parker"; "19"; "B+" ]);
  ]

let conditional_query_tests =
  [
    test "ALTER TABLE"
      (Csv.load ("data" ^ Filename.dir_sep ^ "alter_compare" ^ ".csv"))
      (let _ =
         Csv.save
           ("data" ^ Filename.dir_sep ^ "alter_copy" ^ ".csv")
           (Csv.load ("data" ^ Filename.dir_sep ^ "alter" ^ ".csv"))
       in
       let _ =
         alter_table_add
           [
             ( "alter_copy",
               Csv.load ("data" ^ Filename.dir_sep ^ "alter" ^ ".csv")
             );
           ]
           "alter_copy" "new" STRING
       in
       Csv.load ("data" ^ Filename.dir_sep ^ "alter_copy" ^ ".csv"));
    test "DELETE FROM"
      (Csv.load ("data" ^ Filename.dir_sep ^ "delete_compare" ^ ".csv"))
      (let _ =
         Csv.save
           ("data" ^ Filename.dir_sep ^ "delete_copy" ^ ".csv")
           (Csv.load ("data" ^ Filename.dir_sep ^ "delete" ^ ".csv"))
       in
       let _ =
         delete_table
           [
             ( "delete_copy",
               Csv.load ("data" ^ Filename.dir_sep ^ "delete" ^ ".csv")
             );
           ]
           "delete_copy" "age:int" LESS "25"
       in
       Csv.load ("data" ^ Filename.dir_sep ^ "delete_copy" ^ ".csv"));
    test "UPDATE"
      (Csv.load ("data" ^ Filename.dir_sep ^ "update_compare" ^ ".csv"))
      (let _ =
         Csv.save
           ("data" ^ Filename.dir_sep ^ "update_copy" ^ ".csv")
           (Csv.load ("data" ^ Filename.dir_sep ^ "sample" ^ ".csv"))
       in
       let _ =
         update_table "update_copy"
           [ "id:int"; "name:string"; "age:int" ]
           [ "10"; "josephine"; "27" ]
           "name:string" EQ "cornell"
       in
       Csv.load ("data" ^ Filename.dir_sep ^ "update_copy" ^ ".csv"));
  ]

let sample = Csv.load ("data" ^ Filename.dir_sep ^ "sample" ^ ".csv")

let agg_sample =
  Csv.load ("data" ^ Filename.dir_sep ^ "agg_sample" ^ ".csv")

let aggregate_int_tests =
  [
    test "AGGREGATE INT COUNT" 4
      (aggregate_int_columns
         [ ("sample", sample) ]
         "sample" "age:int" COUNT);
    test "AGGREGATE INT AVERAGE" 41
      (aggregate_int_columns
         [ ("sample", sample) ]
         "sample" "age:int" AVERAGE);
    test "AGGREGATE INT SUM" 165
      (aggregate_int_columns
         [ ("sample", sample) ]
         "sample" "age:int" SUM);
    test "AGGREGATE INT PRODUCT" 900000
      (aggregate_int_columns
         [ ("sample", sample) ]
         "sample" "age:int" PRODUCT);
    test "AGGREGATE INT MIN" 15
      (aggregate_int_columns
         [ ("sample", sample) ]
         "sample" "age:int" MIN);
    test "AGGREGATE INT MAX" 100
      (aggregate_int_columns
         [ ("sample", sample) ]
         "sample" "age:int" MAX);
    test "AGGREGATE STRING CONCAT" "michael, robert, cornell, mesut"
      (aggregate_string_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "name:string" CONCAT);
    test "AGGREGATE STRING CHARACTER_COUNT" "25"
      (aggregate_string_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "name:string" CHARACTER_COUNT);
    test "AGGREGATE STRING WORD_COUNT" "4"
      (aggregate_string_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "name:string" WORD_COUNT);
    test "AGGREGATE BOOLEAN AND" "false"
      (aggregate_boolean_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "deans:bool" AND);
    test "AGGREGATE BOOLEAN NAND" "false"
      (aggregate_boolean_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "deans:bool" NAND);
    test "AGGREGATE BOOLEAN OR" "true"
      (aggregate_boolean_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "deans:bool" OR);
    test "AGGREGATE BOOLEAN NOR" "false"
      (aggregate_boolean_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "deans:bool" NOR);
    test "AGGREGATE BOOLEAN XOR" "false"
      (aggregate_boolean_columns
         [ ("agg_sample", agg_sample) ]
         "agg_sample" "deans:bool" XOR);
  ]

let agg_exceptions =
  [
    test_exceptions "AGGREGATE INT SUM wrong data type as string"
      Database.Parse.Malformed (fun () ->
        aggregate_int_columns
          [ ("sample", sample) ]
          "sample" "name:string" SUM);
    test_exceptions "AGGREGATE INT PRODUCT wrong data type as bool"
      Database.Parse.Malformed (fun () ->
        aggregate_int_columns
          [ ("agg_sample", agg_sample) ]
          "agg_sample" "deans:bool" PRODUCT);
    test_exceptions "AGGREGATE BOOLEAN AND wrong data type as string"
      Database.Parse.Malformed (fun () ->
        aggregate_boolean_columns
          [ ("agg_sample", agg_sample) ]
          "agg_sample" "name:string" AND);
    test_exceptions "AGGREGATE BOOLEAN OR wrong data type as int"
      Database.Parse.Malformed (fun () ->
        aggregate_boolean_columns
          [ ("agg_sample", agg_sample) ]
          "agg_sample" "age:int" OR);
  ]

let table_exceptions =
  [
    test_exceptions "insert wrong datatype int"
      (Stdlib.Failure "Columns do not match values") (fun () ->
        insert "create"
          [
            ("first_name", STRING);
            ("last_name", STRING);
            ("age", INT);
            ("grade", STRING);
          ]
          [ "jenna"; "parker"; "hello"; "B+" ]);
    test_exceptions "insert wrong datatype bool"
      (Stdlib.Failure "Columns do not match values") (fun () ->
        insert "insert_new2"
          [
            ("yes/no", BOOL);
            ("student_name", STRING);
            ("character", CHAR);
            ("account", FLOAT);
          ]
          [ "0"; "dog"; "c"; "1.1" ]);
    test_exceptions "insert wrong datatype char"
      (Stdlib.Failure "Columns do not match values") (fun () ->
        insert "insert_new2"
          [
            ("yes/no", BOOL);
            ("student_name", STRING);
            ("character", CHAR);
            ("account", FLOAT);
          ]
          [ "true"; "dog"; "true"; "1.2" ]);
    test_exceptions "insert wrong datatype float"
      (Stdlib.Failure "Columns do not match values") (fun () ->
        insert "insert_new2"
          [
            ("yes/no", BOOL);
            ("student_name", STRING);
            ("character", CHAR);
            ("account", FLOAT);
          ]
          [ "true"; "dog"; "a"; "hello" ]);
  ]

let test_multiple =
  [
    insert_test "insert_students" "insert_existing"
      [ ("id", INT); ("student_name", STRING); ("grad_year", INT) ]
      [
        [ "11"; "dog"; "2025" ];
        [ "12"; "cat"; "2026" ];
        [ "20"; "tiger"; "2027" ];
      ]
      "insert_compare.csv";
    insert_test "insert_new_test" ""
      [ ("id", INT); ("student_name", STRING); ("grad_year", INT) ]
      [
        [ "0"; "dog"; "2025" ];
        [ "1"; "cat"; "2024" ];
        [ "2"; "tiger"; "2025" ];
        [ "3"; "zebra"; "2024" ];
        [ "4"; "panda"; "2027" ];
      ]
      "insert_new.csv";
    insert_test "insert_new_test" ""
      [
        ("yes/no", BOOL);
        ("student_name", STRING);
        ("character", CHAR);
        ("account", FLOAT);
      ]
      [
        [ "true"; "dog"; "c"; "1.1" ];
        [ "false"; "cat"; "b"; "1.2" ];
        [ "false"; "tiger"; "d"; "1.3" ];
        [ "true"; "zebra"; "e"; "1.4" ];
        [ "false"; "panda"; "f"; "1.5" ];
      ]
      "insert_new2.csv";
  ]

let suite =
  "Test suites"
  >::: List.flatten
         [
           parse_tests;
           parse_exceptions;
           create_drop_tests;
           select_insert_tests;
           conditional_query_tests;
           aggregate_int_tests;
           agg_exceptions;
           table_exceptions;
           test_multiple;
         ]

let _ = run_test_tt_main suite
