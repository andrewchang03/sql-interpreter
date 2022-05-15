open Database
open Parse
open Table
open Csv

let supported_queries =
  [
    "CREATE TABLE";
    "DROP TABLE";
    "SELECT";
    "SELECT FROM";
    "SELECT ALL";
    "INSERT INTO";
    "ALTER TABLE ADD";
    "DELETE FROM";
    "UPDATE";
    "AGGREGATE INT FROM";
    "AGGREGATE STRING FROM";
    "AGGREGATE BOOLEAN FROM";
  ]

let help =
  [
    "SQL queries supported: " ^ String.concat ", " supported_queries;
    "NOTE: The precondtion for any csv files working with this \
     database is that there are no spaces. Multiple words must be \
     connected via _ or through other means.";
    "[load table_name] loads the csv file in data directory";
    "[display table_name] displays the table already loaded in or \
     created";
    "[list] to show list of all tables in the system";
    "[queries help] to show format for queries";
    "[help] to show this message again";
    "[quit] to quit this program";
  ]

let queries_help =
  [
    "[CREATE TABLE table_name col1 col2 ...]";
    "[DROP TABLE table_name] removes table_name";
    "[SELECT col1 col2 ... FROM table_name] grabs col1 col2 ... from \
     table_name and returns them";
    "[SELECT FROM table_name WHERE condition]";
    "[SELECT ALL table_name] returns the entire table";
    "[INSERT INTO table_name col1:data_type col2:data_type ... VALUES \
     val1 val2 ...] inserts val1 val2 ... under corresponding col1 \
     col2 ... with data_type1 data_type2 ...";
    "[ALTER TABLE table_name ADD / DROP / MODIFY column_name datatype]";
    "[DELETE FROM table_name WHERE condition]";
    "[UPDATE table_name col1 col2 ... VALUES val1 val2 ... WHERE \
     condition] updates values under col1 and col2 that match \
     condition with val1 val2 ...";
    "[AGGREGATE INT / STRING / BOOLEAN FROM table_name col_name \
     aggregate_type] accumulates over a specified column.";
    "condition is the format of column_name operator value, and \
     operators can be >, >=, <, <=, =";
  ]

(* merge a string with multiple words into a single string with
   underscore *)
let remove_space s =
  String.concat "_"
    (List.filter
       (fun x -> String.length x > 0)
       (String.split_on_char ' ' s))

(* divide merged string with _ into separate words again *)
let add_space s =
  String.concat " "
    (List.filter
       (fun x -> String.length x > 0)
       (String.split_on_char '_' s))

(* remove_space across the whole table *)
let remove_table_space (table : Csv.t) : Csv.t =
  table |> List.map (fun x -> List.map remove_space x)

(* add_space across the whole table *)
let add_table_space (table : Csv.t) : Csv.t =
  table |> List.map (fun x -> List.map add_space x)

(* for updating table within the repl when the file updates, removes
   table in tables with name = name. *)
let rec update_table_instance tables name =
  match tables with
  | [] -> []
  | h :: t ->
      if fst h = name then update_table_instance t name
      else h :: update_table_instance t name

(* Mutually recursive repl: [loop_repl cmd tables] answers the parser
   and calls the according function, [ask_command tables] prompts the
   user input for SQL command, tables is a live storage for every tables
   imported via load *)
let rec loop_repl (tables : (string * Csv.t) list) :
    (string * Csv.t) list =
  print_newline ();
  print_string "> ";
  let cmd = read_line () in
  match parse cmd with
  | LoadTable name -> load_table name tables (* fully functional *)
  | DropTable name ->
      (* fully functional *)
      if List.mem_assoc name tables then
        Sys.remove ("data/" ^ name ^ ".csv")
      else print_string "table not found.\n";
      loop_repl (drop_table tables name)
  | ListTables ->
      (* fully functional *)
      List.iter (fun x -> print_string (fst x ^ "\n")) tables;
      loop_repl tables
  | DisplayTable name ->
      (* fully functional *)
      print_readable
        (add_table_space
           (snd (List.find (fun x -> fst x = name) tables)));
      loop_repl tables
  | QueriesHelp ->
      (* fully functional *)
      print_string (String.concat "\n" queries_help);
      print_newline ();
      loop_repl tables
  | Help ->
      (* fully functional *)
      print_string (String.concat "\n" help);
      print_newline ();
      loop_repl tables
  | Quit -> Stdlib.exit 0 (* fully functional *)
  | CreateTable s -> begin
      (* fully functional *)
      try
        let data = create_table s.table_name s.cols in
        loop_repl (tables @ [ (s.table_name, data) ])
      with Malformed ->
        print_string "Syntax error with columns list.";
        loop_repl tables
    end
  | Select s ->
      (* fully functional *)
      print_readable
        (add_table_space
           (select
              (snd (List.find (fun x -> fst x = s.table_name) tables))
              s.col_names));
      loop_repl tables
  | SelectWhere s ->
      print_readable
        (select_where_table
           (snd (List.find (fun x -> fst x = s.table_name) tables))
           s.cond.left s.cond.op s.cond.right);
      loop_repl tables
  | SelectAll s -> begin
      (* fully functional *)
      match select_all tables s with
      | t ->
          print_readable t;
          loop_repl tables
      | exception Not_found ->
          print_string "Table not found. Please try again.";
          print_newline ();
          loop_repl tables
    end
  | InsertInto phrase ->
      (* fully functional *)
      loop_repl
        (List.filter (fun x -> fst x <> phrase.table_name) tables
        @ [
            ( phrase.table_name,
              insert phrase.table_name phrase.cols phrase.vals );
          ])
  | AlterTable p -> (
      match p.alt_type with
      | ADD -> (
          match
            alter_table_add tables p.table_name p.col_name p.col_type
          with
          | exception DuplicateName s ->
              print_string
                ("The column name " ^ s
               ^ " already exists. Please choose another");
              loop_repl tables
          | exception NoTable ->
              print_string "This table does not exist in the database.";
              print_newline ();
              loop_repl tables
          | valid_table_list -> loop_repl valid_table_list)
      | MODIFY ->
          loop_repl
            (alter_table_modify tables p.table_name p.col_name
               p.col_type)
      | DROP ->
          loop_repl
            (alter_table_drop tables p.table_name p.col_name p.col_type)
      | UNSUPPORTED s ->
          print_string ("Syntax error at " ^ s);
          loop_repl tables)
  | Delete d -> begin
      (* fully functional *)
      try
        loop_repl
          (delete_table tables d.table_name d.cond.left d.cond.op
             d.cond.right)
      with Malformed -> loop_repl tables
    end
  | Update u -> begin
      try
        loop_repl
          (List.filter (fun x -> fst x <> u.table_name) tables
          @ [
              ( u.table_name,
                update_table u.table_name u.cols u.vals u.cond.left
                  u.cond.op u.cond.right );
            ])
      with Malformed -> loop_repl tables
    end
  | AggInt a ->
      print_int
        (aggregate_int_columns tables a.table_name a.col_name a.agg_type);
      print_newline ();
      loop_repl tables
  | AggString a ->
      print_string
        (aggregate_string_columns tables a.table_name a.col_name
           a.agg_type);
      print_newline ();
      loop_repl tables
  | AggBool a ->
      print_string
        (aggregate_boolean_columns tables a.table_name a.col_name
           a.agg_type);
      print_newline ();
      loop_repl tables
  | exception NoTable ->
      print_string
        "This table does not exist in the database. Please try again.";
      print_newline ();
      loop_repl tables
  | exception Malformed ->
      print_string "Malformed command. Please try again.";
      print_newline ();
      loop_repl tables
(* | _ -> (* for later additional implementations safety net *)
   print_string ""; loop_repl tables *)

and load_table (name : string) (tables : (string * Csv.t) list) :
    (string * Csv.t) list =
  match load ("data/" ^ name ^ ".csv") with
  | d ->
      print_string (name ^ " loaded.");
      print_newline ();
      loop_repl (tables @ [ (name, d) ])
  | exception Sys_error s ->
      print_string "file not found.";
      print_newline ();
      loop_repl tables

(** [main ()] initializes the repl *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nInitializing...\n";
  print_endline "Please enter a command:\n";
  print_string (String.concat "\n" help ^ "\n");
  ignore (loop_repl [])

(* Execute the repl. *)
let () = main ()
