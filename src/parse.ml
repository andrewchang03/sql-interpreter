(* Representation of queries and parsers *)

type data_type =
  | INT
  | FLOAT
  | BOOL
  | STRING
  | CHAR
  | UNSUPPORTED of string

type alter_type =
  | ADD
  | DROP
  | MODIFY
  | UNSUPPORTED of string

(* ALTER TABLE table_name ADD column_name datatype *)
type alter_phrase = {
  table_name : string;
  alt_type : alter_type;
  col_name : string;
  col_type : data_type;
}

type operator =
  | LESS
  | GREATER
  | EQ
  | LE
  | GE
  | UNSUPPORTED of string

type aggregate_int =
  | AVERAGE
  | MEDIAN
  | SUM
  | PRODUCT
  | MIN
  | MAX
  | COUNT

type aggregate_string =
  | CONCAT
  | CHARACTER_COUNT
  | WORD_COUNT

type aggregate_boolean =
  | AND
  | NAND
  | OR
  | NOR
  | XOR

(* left op right *)
type condition = {
  left : string;
  op : operator;
  right : string;
}

(* QUERY PHRASES *)

(* INSERT INTO table_name col1 col2 ... VALUES val1 val2 ... *)
type insert_phrase = {
  table_name : string;
  cols : (string * data_type) list;
  vals : string list;
}

(* SELECT col1 col2 ... FROM table_name *)
type select_phrase = {
  table_name : string;
  col_names : string list; (* [col1; col2; ...] *)
}

(* SELECT col1 col2 ... FROM table_name WHERE condition *)
type select_where_phrase = {
  table_name : string;
  cond : condition;
}

(* UPDATE table_name col1 col2 ... VALUES val1 val2 ... WHERE
   condition *)
type update_phrase = {
  table_name : string;
  cols : string list;
  vals : string list;
  cond : condition;
}

(* DELETE FROM table_name WHERE condition *)
type delete_phrase = {
  table_name : string;
  cond : condition;
}

(* CREATE TABLE table_name col1 col2 ... *)
type create_phrase = {
  table_name : string;
  cols : (string * data_type) list;
}

type aggregate_int_phrase = {
  table_name : string;
  col_name : string;
  agg_type : aggregate_int;
}

type aggregate_string_phrase = {
  table_name : string;
  col_name : string;
  agg_type : aggregate_string;
}

type aggregate_bool_phrase = {
  table_name : string;
  col_name : string;
  agg_type : aggregate_boolean;
}

(* TYPE OF COMMANDS *)

type command =
  | CreateTable of create_phrase
  | DropTable of string
  | AlterTable of alter_phrase
  | Select of select_phrase
  | SelectWhere of select_where_phrase
  | SelectAll of string
  | InsertInto of insert_phrase
  | Update of update_phrase
  | Delete of delete_phrase
  | AggInt of aggregate_int_phrase
  | AggString of aggregate_string_phrase
  | AggBool of aggregate_bool_phrase
  | LoadTable of string
  | DisplayTable of string
  | CopyFile of string
  | ListTables
  | Help
  | QueriesHelp
  | Quit

exception Empty
exception Malformed
exception DuplicateName of string
exception NoTable

(* PARSER FUNCTIONS *)
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
  | "FROM" :: t_name :: t -> t_name
  | _ :: t -> parse_from_table t

let rec index (lst : string list) (elem : string) : int =
  match lst with
  | [] -> -1
  | h :: t -> if h = elem then 0 else 1 + index t elem

let rec get_items_before (lst : string list) (stop_word : string) :
    string list =
  if index lst stop_word = -1 then raise Malformed;
  match lst with
  | [] -> raise Malformed
  | h :: s :: _ when s = stop_word -> [ h ]
  | h :: t -> h :: get_items_before t stop_word

let rec get_items_between
    (lst : string list)
    (start_word : string)
    (stop_word : string) =
  let index_start = index lst start_word in
  let index_stop = index lst stop_word in
  if index_start = -1 || index_stop = -1 || index_start >= index_stop
  then raise Malformed;
  match lst with
  | [] -> raise Malformed
  | start :: t when start = start_word -> get_items_before t stop_word
  | _ :: t -> get_items_between t start_word stop_word

let rec get_items_after (lst : string list) (start_word : string) :
    string list =
  if index lst start_word = -1 then raise Malformed;
  match lst with
  | [] -> raise Malformed
  | s :: t when s = start_word -> t
  | _ :: t -> get_items_after t start_word

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

let parse_select (lst : string list) : select_phrase =
  {
    table_name = parse_from_table lst;
    col_names = get_items_before lst "FROM";
  }

let parse_select_where (lst : string list) : select_where_phrase =
  {
    table_name = parse_from_table lst;
    cond = parse_condition (get_items_after lst "WHERE");
  }

(** [parse_cols cols] takes in a list of columns [cols] formatted as [\[
    colname1 datatype; colname2 datatype; ... \]] and converts them into
    [\[\ (colname1, datatype); (colname2, datatype), ... ]] *)
let parse_cols (cols : string list) : (string * data_type) list =
  List.map
    (fun col ->
      let col_data = String.split_on_char ':' col in
      match col_data with
      | [ n; t ] ->
          let dt =
            match t with
            | "int" -> INT
            | "float" -> FLOAT
            | "bool" -> BOOL
            | "char" -> CHAR
            | "string" -> STRING
            | _ -> UNSUPPORTED t
          in
          (n, dt)
      | _ -> raise Malformed)
    cols

let parse_insert (t_name : string) (lst : string list) : insert_phrase =
  let cols = get_items_before lst "VALUES" in
  let vals = get_items_after lst "VALUES" in
  if List.length cols <> List.length vals then raise Malformed
  else { table_name = t_name; cols = parse_cols cols; vals }

let parse_update (t_name : string) (lst : string list) : update_phrase =
  {
    table_name = t_name;
    cols = get_items_before lst "VALUES";
    vals = get_items_between lst "VALUES" "WHERE";
    cond = parse_condition (get_items_after lst "WHERE");
  }

let parse_delete (t_name : string) (lst : string list) : delete_phrase =
  { table_name = t_name; cond = parse_condition lst }

let rec create_parse_cols (lst : string list) :
    (string * data_type) list =
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
      (name, dt) :: create_parse_cols t
  | _ -> raise Malformed

let rec parse_from_table (lst : string list) : string =
  match lst with
  | [] -> raise Malformed
  | [ "FROM"; t_name ] -> t_name
  | _ :: t -> parse_from_table t

let parse_select (lst : string list) : select_phrase =
  {
    table_name = parse_from_table lst;
    col_names = get_items_before lst "FROM";
  }

let parse_select_where (table_name : string) (cond_lst : string list) :
    select_where_phrase =
  { table_name; cond = parse_condition cond_lst }

let parse_create (table_name : string) (cols : string list) :
    create_phrase =
  match cols with
  | h :: t -> { table_name; cols = create_parse_cols cols }
  | [] -> raise Malformed

let parse_alter
    (table_name : string)
    (alter_type : string)
    (column_name : string)
    (column_type : string) : alter_phrase =
  {
    table_name;
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

let parse_insert (table_name : string) (lst : string list) :
    insert_phrase =
  let cols = get_items_before lst "VALUES" |> parse_cols in
  let vals = get_items_after lst "VALUES" in
  if List.length cols <> List.length vals then raise Malformed
  else { table_name; cols; vals }

let parse_update (table_name : string) (lst : string list) :
    update_phrase =
  {
    table_name;
    cols = get_items_before lst "VALUES";
    vals = get_items_between lst "VALUES" "WHERE";
    cond = parse_condition (get_items_after lst "WHERE");
  }

let parse_delete (table_name : string) (lst : string list) :
    delete_phrase =
  { table_name; cond = parse_condition lst }

let parse (str : string) : command =
  match
    List.filter
      (fun s -> String.length s > 0)
      (String.split_on_char ' ' str)
  with
  | [ "load"; table_name ] -> LoadTable table_name
  | [ "display"; table_name ] -> DisplayTable table_name
  | [ "copy"; file_name ] -> CopyFile file_name
  | [ "list" ] -> ListTables
  | [ "queries"; "help" ] -> QueriesHelp
  | [ "help" ] -> Help
  | [ "quit" ] -> Quit
  | [ "SELECT"; "ALL"; t ] -> SelectAll t
  | "SELECT" :: "FROM" :: table_name :: "WHERE" :: t ->
      SelectWhere (parse_select_where table_name t)
  | "SELECT" :: t -> Select (parse_select t)
  | "CREATE" :: "TABLE" :: table_name :: t ->
      CreateTable (parse_create table_name t)
  | [ "DROP"; "TABLE"; t ] -> DropTable t
  | [ "ALTER"; "TABLE"; table_name; alter_type; col_name; col_type ] ->
      AlterTable (parse_alter table_name alter_type col_name col_type)
  | "INSERT" :: "INTO" :: table_name :: t ->
      InsertInto (parse_insert table_name t)
  | "UPDATE" :: table_name :: t -> Update (parse_update table_name t)
  | "DELETE" :: "FROM" :: table_name :: "WHERE" :: t ->
      Delete (parse_delete table_name t)
  | "AGGREGATE"
    :: "INT" :: "FROM" :: table_name :: col_name :: agg_type :: t ->
      AggInt
        {
          table_name;
          col_name;
          agg_type =
            begin
              match agg_type with
              | "average" -> AVERAGE
              | "median" -> MEDIAN
              | "sum" -> SUM
              | "product" -> PRODUCT
              | "min" -> MIN
              | "max" -> MAX
              | "count" -> COUNT
              | _ -> raise Malformed
            end;
        }
  | "AGGREGATE"
    :: "STRING" :: "FROM" :: table_name :: col_name :: agg_type :: t ->
      AggString
        {
          table_name;
          col_name;
          agg_type =
            begin
              match agg_type with
              | "concat" -> CONCAT
              | "charcount" -> CHARACTER_COUNT
              | "wordcount" -> WORD_COUNT
              | _ -> raise Malformed
            end;
        }
  | "AGGREGATE"
    :: "BOOLEAN" :: "FROM" :: table_name :: col_name :: agg_type :: t ->
      AggBool
        {
          table_name;
          col_name;
          agg_type =
            begin
              match agg_type with
              | "and" -> AND
              | "nand" -> NAND
              | "or" -> OR
              | "nor" -> NOR
              | "xor" -> XOR
              | _ -> raise Malformed
            end;
        }
  | [] -> raise Empty
  | _ -> raise Malformed