(** Implementation of database command parser modeled after SQL. *)

(* INSERT INTO table_name col1 col2 ... VALUES val1 val2 ... *)
type insert_phrase = {
  table_name : string;
  col_names : string list; (* [col1; col2; ...] *)
  vals : string list; (* [val1; val2; ...] *)
}

type alter_type =
  | ADD
  | DROP
  | MODIFY
  | UNSUPPORTED of string

type data_type =
  | INT
  | FLOAT
  | BOOL
  | STRING
  | CHAR
  | UNSUPPORTED of string

(* ALTER TABLE table_name ADD column_name datatype *)
type alter_phrase = {
  table_name : string;
  alt_type : alter_type;
  col_name : string;
  col_type : data_type;
}

(* SELECT col1 col2 ... FROM table_name *)
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
  | UNSUPPORTED of string

(* left op right *)
type condition = {
  left : string;
  right : string;
  op : operator;
}

(* UPDATE table_name col1 col2 ... VALUES val1 val2 ... WHERE
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

(* CREATE TABLE table_name col1 datatype col2 datatype ... *)
type create_phrase = {
  table_name : string;
  cols : (string * data_type) list;
}

type command =
  | CreateTable of create_phrase
  | DropTable of string
  | AlterTable of alter_phrase
  | Select of select_phrase
  | SelectAll of string
  | InsertInto of insert_phrase
  | Update of update_phrase
  | Delete of delete_phrase
  | LoadTable of string
  | DisplayTable of string
  | ListTables
  | Help
  | Quit

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
        | _ -> UNSUPPORTED alter_type
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
        | _ -> UNSUPPORTED column_type
      end;
  }

let rec parse_from_table lst =
  match lst with
  | [] -> raise Malformed
  | [ "FROM"; t_name ] -> t_name
  | _ :: t -> parse_from_table t

let rec index lst elem =
  match lst with
  | [] -> -1
  | h :: t -> if h = elem then 0 else 1 + index t elem

let rec get_items_before lst stop_word =
  if index lst stop_word = -1 then raise Malformed;
  match lst with
  | [] -> raise Malformed
  | h :: s :: _ when s = stop_word -> [ h ]
  | h :: t -> h :: get_items_before t stop_word

let rec get_items_after lst start_word =
  if index lst start_word = -1 then raise Malformed;
  match lst with
  | [] -> raise Malformed
  | s :: t when s = start_word -> t
  | _ :: t -> get_items_after t start_word

let rec get_items_between lst start_word stop_word =
  let index_start = index lst start_word in
  let index_stop = index lst stop_word in
  if index_start = -1 || index_stop = -1 || index_start >= index_stop
  then raise Malformed;
  match lst with
  | [] -> raise Malformed
  | start :: t when start = start_word -> get_items_before t stop_word
  | _ :: t -> get_items_between t start_word stop_word

let parse_select (lst : string list) : select_phrase =
  {
    table_name = parse_from_table lst;
    col_names = get_items_before lst "FROM";
  }

let parse_insert (t_name : string) (lst : string list) : insert_phrase =
  let cols = get_items_before lst "VALUES" in
  let vals = get_items_after lst "VALUES" in
  if List.length cols <> List.length vals then raise Malformed
  else { table_name = t_name; col_names = cols; vals }

let parse_condition = function
  | [ l; o; r ] ->
      {
        left = l;
        right = r;
        op =
          (if o = "<" then LESS
          else if o = ">" then GREATER
          else if o = "=" then EQ
          else if o = "<=" then LE
          else if o = ">=" then GE
          else UNSUPPORTED o);
      }
  | _ -> raise Malformed

let parse_update (t_name : string) (lst : string list) : update_phrase =
  {
    table_name = t_name;
    col_names = get_items_before lst "VALUES";
    vals = get_items_between lst "VALUES" "WHERE";
    cond = parse_condition (get_items_after lst "WHERE");
  }

let parse_delete (t_name : string) (lst : string list) : delete_phrase =
  { table_name = t_name; cond = parse_condition lst }

let rec parse_cols (lst : string list) : (string * data_type) list =
  match lst with
  | [] -> []
  | name :: data :: t ->
      let dt =
        match data with
        | "int" -> INT
        | "float" -> FLOAT
        | "bool" -> BOOL
        | "string" -> STRING
        | "char" -> CHAR
        | _ -> UNSUPPORTED data
      in
      (name, dt) :: parse_cols t
  | _ -> raise Malformed

let parse_create (t_name : string) (lst : string list) : create_phrase =
  try { table_name = t_name; cols = parse_cols lst }
  with Malformed -> raise Malformed

let parse str =
  match
    List.filter
      (fun s -> String.length s > 0)
      (String.split_on_char ' ' str)
  with
  | "CREATE" :: "TABLE" :: table_name :: t ->
      CreateTable (parse_create table_name t)
  | [ "DROP"; "TABLE"; t ] -> DropTable t
  | [ "ALTER"; "TABLE"; table_name; alter_type; col_name; col_type ] ->
      AlterTable (parse_alter table_name alter_type col_name col_type)
  | [ "SELECT"; "ALL"; t ] -> SelectAll t
  | "SELECT" :: t -> Select (parse_select t)
  | "INSERT" :: "INTO" :: table_name :: t ->
      InsertInto (parse_insert table_name t)
  | "UPDATE" :: table_name :: t -> Update (parse_update table_name t)
  | "DELETE" :: "FROM" :: table_name :: "WHERE" :: t ->
      Delete (parse_delete table_name t)
  | [ "load"; table_name ] -> LoadTable table_name
  | [ "display"; table_name ] -> DisplayTable table_name
  | [ "list" ] -> ListTables
  | [ "help" ] -> Help
  | [ "quit" ] -> Quit
  | [] -> raise Empty
  | _ -> raise Malformed
