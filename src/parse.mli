(** Provides variant types and ASTs for queries along with a parser *)

(** [data_type] provides variant data types that the table supports *)
type data_type =
  | INT
  | FLOAT
  | BOOL
  | STRING
  | CHAR
  | UNSUPPORTED of string

(** [alter_type] provides a list of variant alter operations *)
type alter_type =
  | ADD
  | DROP
  | MODIFY
  | UNSUPPORTED of string

(** [operator] provides variant types for operators to be used in
    conditions *)
type operator =
  | LESS
  | GREATER
  | EQ
  | LE
  | GE
  | UNSUPPORTED of string

(** [aggregate_int] provides variant operations for integer columns *)
type aggregate_int =
  | AVERAGE
  | MEDIAN
  | SUM
  | PRODUCT
  | MIN
  | MAX
  | COUNT

(** [aggregate_string] provides the variant operations for string
    columns *)
type aggregate_string =
  | CONCAT
  | CHARACTER_COUNT
  | WORD_COUNT

(** [aggregate_boolean] provides the variant operations for bool columns *)
type aggregate_boolean =
  | AND
  | NAND
  | OR
  | NOR
  | XOR

type condition = {
  left : string;
  op : operator;
  right : string;
}
(** [condition] specifies the format of conditions in queries *)

type insert_phrase = {
  table_name : string;
  cols : (string * data_type) list;
  vals : string list;
}
(** [insert_phrase] provides the AST for INSERT INTO queries *)

type alter_phrase = {
  table_name : string;
  alt_type : alter_type;
  col_name : string;
  col_type : data_type;
}
(** [alter_phrase] provides the AST for ALTER TABLE queries *)

type select_phrase = {
  table_name : string;
  col_names : string list;
}
(** [select_phrase] provides the AST for SELECT queries *)

type select_where_phrase = {
  table_name : string;
  cond : condition;
}

type update_phrase = {
  table_name : string;
  cols : string list;
  vals : string list;
  cond : condition;
}
(** [update_phrase] provides the AST for UPDATE queries *)

type delete_phrase = {
  table_name : string;
  cond : condition;
}
(** [delete_phrase] provides the AST for DELETE FROM queries *)

type create_phrase = {
  table_name : string;
  cols : (string * data_type) list;
}
(** [create_phrase] provides the AST for CREATE TABLE queries *)

type aggregate_int_phrase = {
  table_name : string;
  col_name : string;
  agg_type : aggregate_int;
}
(** [aggregate_int_phrase] provides the AST for aggregate operations on
    columns holding integer data *)

type aggregate_string_phrase = {
  table_name : string;
  col_name : string;
  agg_type : aggregate_string;
}
(** [aggregate_string_phrase] provides the AST for aggregate operations
    on columns holding string data *)

type aggregate_bool_phrase = {
  table_name : string;
  col_name : string;
  agg_type : aggregate_boolean;
}
(** [aggregate_bool_phrase] provides the AST for aggregate operations on
    columns holding boolean data *)

(** [command] provides a series of command or query variants *)
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
  | ListTables
  | Help
  | QueriesHelp
  | Quit

exception Empty
(** [raise Empty] when table is empty *)

exception Malformed
(** [raise Malformed] when the query cannot be parsed correctly *)

exception DuplicateName of string
(** [raise DuplicateName s] when there are multiple tables with name [s] *)

exception NoTable
(** [raise NoTable] when there is no table in database. *)

val parse : string -> command
(** [parse s] parses user-provided query [s] into an according command *)