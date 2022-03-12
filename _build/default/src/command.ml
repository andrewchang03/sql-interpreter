(* Note: You may introduce new code anywhere in this file. *)

type insert_phrase = {
   table_name: string;
   col_names: string list;
   vals: string list;
 }

 type alter_type = ADD | DROP | MODIFY

 type alter_phrase = {
   table_name: string;
   alt_type: alter_type;
   col_name: string;
   col_type: string;
 }

 type select_phrase = {
   table_name: string;
   col_names: string list;
 }

 type operator = LESS | GREATER | EQ | LE | GE

 type condition = {
   left: string;
   right: string;
   op: operator;
 }
 
 type update_phrase = {
   table_name: string;
   col_names: string list;
   vals: string list;
   cond: condition;
 }

 type delete_phrase = {
   table_name: string;
   cond: condition;
 }

type command =
| CreateTable of string 
| DropTable of string 
| AlterTable of alter_phrase 
| Select of select_phrase
| SelectAll of string
| InsertInto of insert_phrase 
| Update of update_phrase
| Delete of delete_phrase
| Quit

exception Empty
exception Malformed

let parse_select (t:string list) : select_phrase =
  {table_name = "", col_names = []}

let parse str =
  match
    List.filter
      (fun s -> String.length s > 0)
      (String.split_on_char ' ' str)
  with
  | "CREATE" :: "TABLE" :: [t] -> CreateTable of t
  | "DROP" :: "TABLE" :: [t] -> DropTable of t
  | "ALTER" :: "TABLE" :: [t] -> AlterTable of t
  | "SELECT" :: "ALL" :: [t] -> SelectAll of t
  | "SELECT" :: t -> Select of parse_select(t)
  | [ "quit" ] -> Quit
  | [] -> raise Empty
  | _ -> raise Malformed
