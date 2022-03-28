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

type operator =
  | LESS
  | GREATER
  | EQ
  | LE
  | GE
  | UNSUPPORTED of string

type condition = {
  left : string;
  right : string;
  op : operator;
}

(* QUERIES PHRASES *)

type insert_phrase = {
  table_name : string;
  col_names : string list;
  vals : string list;
}

type alter_phrase = {
  table_name : string;
  alt_type : alter_type;
  col_name : string;
  col_type : data_type;
}

type select_phrase = {
  table_name : string;
  col_names : string list;
}

type update_phrase = {
  table_name : string;
  col_names : string list;
  vals : string list;
  cond : condition;
}

type delete_phrase = {
  table_name : string;
  cond : condition;
}

type create_phrase = {
  table_name : string;
  cols : (string * data_type) list;
}

(* COMMANDS *)

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
  | QueriesHelp
  | Help
  | Quit

exception Empty
exception Malformed
exception NoTable

val parse : string -> command