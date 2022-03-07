(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty
exception Malformed

let parse str =
  match
    List.filter
      (fun s -> String.length s > 0)
      (String.split_on_char ' ' str)
  with
  | "go" :: t -> Go t
  | [ "quit" ] -> Quit
  | [] -> raise Empty
  | _ -> raise Malformed
