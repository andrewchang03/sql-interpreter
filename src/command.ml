(* Note: You may introduce new code anywhere in this file. *)

type insert_phrase = {
  table_name : string;
  col_names : string list;
  vals : string list;
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

type select_phrase = {
  table_name : string;
  col_names : string list;
}

type operator =
  | LESS
  | GREATER
  | EQ
  | LE
  | GE
  | MALFORMED of string

type condition = {
  left : string;
  right : string;
  op : operator;
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
