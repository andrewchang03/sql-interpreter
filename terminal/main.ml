open Database
open Command
open Table
open Csv
open Table

let supported_queries =
  [
    "CREATE TABLE";
    "DROP TABLE";
    "SELECT";
    "SELECT ALL";
    "INSERT INTO";
    "DELETE FROM";
    "UPDATE";
    "ALTER TABLE";
  ]

let help =
  [
    "SQL queries supported: " ^ String.concat " " supported_queries;
    "NOTE: table names must be a single string without spaces, column \
     names and values must be connected via _ if multiple words.";
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
    "[CREATE TABLE table_name col1 datatype col2 datatype ...]";
    "[DROP TABLE table_name] removes table_name";
    "[SELECT col1 col2 ... FROM table_name] grabs col1 col2 ... from \
     table_name and returns them";
    "[SELECT ALL table_name] returns the entire table";
    "[INSERT INTO table_name col1_type col2_type ... VALUES val1 val2 \
     ...] inserts val1 val2 ... under corresponding col1 col2 ... \
     where type represents the col type into table_name";
    "[DELETE FROM table_name WHERE condition]";
    "[UPDATE table_name col1 col2 ... VALUES val1 val2 ... WHERE\n\
    \    condition] updates values under col1 and col2 that match \
     condition with val1 val2 ...";
    "[ALTER TABLE table_name ADD column_name datatype]";
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

(* remove_space across the whole table, this is necessary because our
   parser divides on spaces *)
let remove_table_space table =
  List.map (fun x -> List.map remove_space x) table

(* add_space across the whole table *)
let add_table_space table =
  List.map (fun x -> List.map add_space x) table

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
  | LoadTable name -> load_table name tables
  | DropTable name ->
      if List.mem_assoc name tables then
        Sys.remove ("data/" ^ name ^ ".csv")
      else print_string "table not found.\n";
      loop_repl (drop_table tables name)
  | QueriesHelp ->
      print_string (String.concat "\n" queries_help);
      print_newline ();
      loop_repl tables
  | Help ->
      print_string (String.concat "\n" help);
      print_newline ();
      loop_repl tables
  | Quit -> Stdlib.exit 0
  | CreateTable s -> begin
      try
        let data = create_table s.table_name s.cols in
        loop_repl (tables @ [ (s.table_name, data) ])
      with Malformed ->
        print_string "Syntax error with columns list.";
        loop_repl tables
    end
  | Select s ->
      print_readable
        (add_table_space
           (select
              (snd (List.find (fun x -> fst x = s.table_name) tables))
              s.col_names));
      loop_repl tables
  | SelectAll s -> begin
      match select_all tables s with
      | t ->
          print_readable t;
          loop_repl tables
      | exception Not_found ->
          print_string "Table not found. Please try again.";
          print_newline ();
          loop_repl tables
    end
  | InsertInto phrase -> begin
      try
        let data = insert phrase.table_name phrase.cols phrase.vals in
        (* let data = load ("data/" ^ phrase.table_name ^ ".csv") in *)
        loop_repl
          (update_table_instance tables phrase.table_name
          @ [ (phrase.table_name, data) ])
      with Stdlib.Failure f ->
        print_string "Incorrect data type.";
        loop_repl tables
    end
  | DisplayTable name ->
      print_readable
        (add_table_space
           (snd (List.find (fun x -> fst x = name) tables)));
      loop_repl tables
  | ListTables ->
      List.iter (fun x -> print_string (fst x ^ "\n")) tables;
      loop_repl tables
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
              loop_repl tables
          | valid_table_list -> loop_repl valid_table_list)
      | MODIFY ->
          alter_table_modify tables p.table_name p.col_name p.col_type
      | DROP ->
          loop_repl
            (alter_table_drop tables p.table_name p.col_name p.col_type)
      | UNSUPPORTED s ->
          print_string ("Syntax\n         error at " ^ s);
          loop_repl tables)
  | exception Malformed ->
      print_string "Malformed command. Please try again.";
      print_newline ();
      loop_repl tables
  | _ ->
      print_string "";
      loop_repl tables

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
