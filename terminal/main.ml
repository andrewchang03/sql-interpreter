open Database
open Command
open Table
open Csv
open Table

let help =
  "SQL queries supported: DROP TABLE, SELECT, SELECT ALL\n\
   [load table_name] loads the csv file in data directory\n\
   [display table_name] displays the table already loaded in or created\n\
   [list] to show list of all tables in the system\n\
   [help] to show this message again\n\
   [quit] to quit this program\n"

(* Mutually recursive repl: [loop_repl cmd tables] answers the parser
   and calls the according function, [ask_command tables] prompts the
   user input for SQL command, tables is a live storage for every tables
   imported via load *)
let rec loop_repl (tables : (string * Csv.t) list) =
  print_string "\n> ";
  let cmd = read_line () in
  match parse cmd with
  | DropTable name -> loop_repl (drop_table tables name)
  | LoadTable name ->
      let data = load ("data/" ^ name ^ ".csv") in
      loop_repl (tables @ [ (name, data) ])
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
        (select
           (snd (List.find (fun x -> fst x = s.table_name) tables))
           s.col_names);
      loop_repl tables
  | SelectAll s ->
      print_readable (select_all tables s);
      loop_repl tables
  | DisplayTable name ->
      print_readable (snd (List.find (fun x -> fst x = name) tables));
      loop_repl tables
  | ListTables ->
      List.iter (fun x -> print_string (fst x ^ "\n")) tables;
      loop_repl tables
  | AlterTable p -> begin
      match p.alt_type with
      | ADD -> alter_table_add tables p.table_name p.col_name p.col_type
      | MODIFY ->
          alter_table_modify tables p.table_name p.col_name p.col_type
      | DROP ->
          alter_table_drop tables p.table_name p.col_name p.col_type
      | UNSUPPORTED s ->
          print_string ("Syntax error at " ^ s);
          loop_repl tables
    end
  | Help ->
      print_string help;
      loop_repl tables
  | Quit -> Stdlib.exit 0
  | _ ->
      print_string "";
      loop_repl tables

(** [main ()] initializes the repl *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nInitializing...\n";
  print_endline "Please enter a command:\n";
  print_string help;
  ignore (loop_repl [])

(* Execute the repl. *)
let () = main ()
