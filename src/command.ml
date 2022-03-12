(** Implementation of database command parser modeled after SQL. *)

(* INSERT INTO table_name (col1, col2, ...) VALUES (val1, val2, ...) *)
type insert_phrase = {
  table_name : string;
  col_names : string list; (* [col1; col2; ...] *)
  vals : string list; (* [val1; val2; ...] *)
}

type alter_type =
  | ADD
  | DROP
  | MODIFY
  | MALFORMED of string

type data_type =
  | INT
  | FLOAT
  | BOOL
  | STRING
  | CHAR
  | MALFORMED of string

type alter_phrase = {
  table_name : string;
  alt_type : alter_type;
  col_name : string;
  col_type : data_type;
}

(* SELECT col1, col2,... FROM table_name *)
type select_phrase = {
  table_name : string;
  col_names : string list; (* [col1; col2; ...] *)
}

type operator =
  | LESS
  | GREATER
  | EQ
  | LE
  | GE
  | MALFORMED of string

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
  col_names : string list; (* [col1; col2; ...] *)
  vals : string list; (* [val1; val2; ...] *)
  cond : condition;
}

(* DELETE FROM table_name WHERE condition *)
type delete_phrase = {
  table_name : string;
  cond : condition;
}

type operator =
  | LESS
  | GREATER
  | EQ
  | LEQ
  | GEQ

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

let create_table = failwith "Unimplemented: create_table"
let drop_table = failwith "Unimplemented: drop_table"
let select_op = failwith "Unimplemented: select_op"
let insert_op = failwith "Unimplemented: insert_op"
let update_op = failwith "Unimplemented: update_op"
let delete_op = failwith "Unimplemented: delete_op"

exception Empty
exception Malformed
exception NoTable

let parse_alter t_name alter_type column_name column_type : alter_phrase
    =
  {
    table_name = t_name;
    alt_type =
      begin
        match alter_type with
        | "ADD" -> ADD
        | "DROP" -> DROP
        | "MODIFY" -> MODIFY
        | _ -> MALFORMED alter_type
      end;
    col_name = column_name;
    col_type =
      begin
        match column_type with
        | "int" -> INT
        | "float" -> FLOAT
        | "bool" -> BOOL
        | "char" -> CHAR
        | "string" -> STRING
        | _ -> MALFORMED column_type
      end;
  }

let parse_select (t : string list) : select_phrase =
  { table_name = ""; col_names = [] }

let parse_insert (t : string list) : insert_phrase =
  { table_name = ""; col_names = []; vals = [] }

let parse_condition (t : string list) : condition =
  { left = ""; right = ""; op = LESS }

let parse_update (t : string list) : update_phrase =
  {
    table_name = "";
    col_names = [];
    vals = [];
    cond = parse_condition t;
  }

let parse_delete (t : string list) : delete_phrase =
  { table_name = ""; cond = parse_condition t }

let parse str =
  match
    List.filter
      (fun s -> String.length s > 0)
      (String.split_on_char ' ' str)
  with
  | [ "CREATE"; "TABLE"; t ] -> CreateTable t
  | [ "DROP"; "TABLE"; t ] -> DropTable t
  | [ "ALTER"; "TABLE"; table_name; alter_type; col_name; col_type ] ->
      AlterTable (parse_alter table_name alter_type col_name col_type)
  | "SELECT" :: "ALL" :: [ t ] -> SelectAll t
  | "SELECT" :: t -> Select (parse_select t)
  | "INSERT" :: "INTO" :: t -> InsertInto (parse_insert t)
  | "UPDATE" :: t -> Update (parse_update t)
  | "DELETE" :: t -> Delete (parse_delete t)
  | [ "quit" ] -> Quit
  | [] -> raise Empty
  | _ -> raise Malformed
