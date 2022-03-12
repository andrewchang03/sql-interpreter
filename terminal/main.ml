open Database
open Command

let rec loop_repl cmd = failwith "Unimplemented: loop_repl"

let ask_command () =
  print_string "\n> ";
  let cmd = read_line () in
  loop_repl cmd

(** [main ()] initializes the repl *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nInitializing...\n";
  print_endline "Please enter a command:";
  ask_command ()

(* Execute the repl. *)
let () = main ()
