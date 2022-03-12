(** Implementation of database command parser modeled after SQL. *)

type object_phrase = string list

(* SELECT col1, col2,... FROM table_name *)
type select_phrase = {
  table_name : string;
  cols : string list; (* [col1; col2; ...] *)
}

(* INSERT INTO table_name (col1, col2, ...) VALUES (val1, val2, ...) *)
type insert_phrase = {
  table_name : string;
  cols : string list; (* [col1; col2; ...] *)
  vals : string list; (* [val1; val2; ...] *)
}

type operator =
  | LESS
  | GREATER
  | EQ
  | LEQ
  | GEQ

(* left op right *)
type condition = {
  left : string;
  right : string;
  op : operator;
}

(* UPDATE table_name SET col1 = val1, col2 = val2, ... WHERE
   condition *)
type update_phrase = {
  table_name : string;
  cols : string list; (* [col1; col2; ...] *)
  vals : string list; (* [val1; val2; ...] *)
  cond : condition;
}

(* DELETE FROM table_name WHERE condition *)
type delete_phrase = {
  table_name : string;
  cond : condition;
}

type command =
  | CreateTable of string (* CREATE TABLE my_table *)
  | DropTable of string (* DROP TABLE my_table *)
  | Select of select_phrase (* SELECT col1, col2,... FROM table_name *)
  | InsertInto of insert_phrase
  (* INSERT INTO table_name (col1, col2, ...) VALUES (val1, val2,
     ...) *)
  | Update of update_phrase
  (* UPDATE table_name SET col1 = val1, col2 = val2, ... WHERE
     condition *)
  | Delete of delete_phrase
(* DELETE FROM table_name WHERE condition *)

let create_table = failwith "Unimplemented: create_table"
let drop_table = failwith "Unimplemented: drop_table"
let select_op = failwith "Unimplemented: select_op"
let insert_op = failwith "Unimplemented: insert_op"
let update_op = failwith "Unimplemented: update_op"
let delete_op = failwith "Unimplemented: delete_op"

exception Empty
exception Malformed
exception NoTable

let parse str =
  match
    List.filter
      (fun s -> String.length s > 0)
      (String.split_on_char ' ' str)
  with
  | _ -> failwith "Unimplemented: parse"
