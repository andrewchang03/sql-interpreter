open Database
open Command
open Csv
open Queries

let help =
  "SQL queries supported: N/A\n\
   [load table_name] loads the csv file in data directory\n\
   [display table_name] displays the table already loaded in or created\n\
   [list] to show list of all tables in the system\n\
   [help] to show this message again\n\
   [quit] to quit this program\n"

let rec loop_repl cmd tables =
  match parse cmd with
  | DropTable name ->
      ask_command (List.filter (fun x -> fst x <> name) tables)
  | LoadTable name ->
      let data = load ("data/" ^ name ^ ".csv") in
      ask_command (tables @ [ (name, data) ])
  | Select s ->
      print_readable
        (select
           (snd (List.find (fun x -> fst x = s.table_name) tables))
           s.col_names);
      ask_command tables
  | DisplayTable name ->
      print_readable (snd (List.find (fun x -> fst x = name) tables));
      ask_command tables
  | ListTables ->
      List.iter (fun x -> print_string (fst x ^ "\n")) tables;
      ask_command tables
  | Help ->
      print_string help;
      ask_command tables
  | Quit -> Stdlib.exit 0
  | _ ->
      print_string "";
      ask_command tables

and ask_command tables =
  print_string "\n> ";
  let cmd = read_line () in
  loop_repl cmd tables

(** [main ()] initializes the repl *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nInitializing...\n";
  print_endline "Please enter a command:\n";
  print_string help;
  ask_command []

(* Execute the repl. *)
let () = main ()
