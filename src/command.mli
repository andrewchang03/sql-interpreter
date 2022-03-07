(** Parsing of database commands, modeled after SQL language. *)

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

 (** The type [object_phrase] represents the object phrase that can be
     part of a player command. Each element of the list represents a word
     of the object phrase, where a {i word} is defined as a consecutive
     sequence of non-space characters. Thus, no element of the list
     should contain any leading, internal, or trailing spaces. The list
     is in the same order as the words in the original player command.
     For example:
 
     - If the player command is ["go clock tower"], then the object
       phrase is [\["clock"; "tower"\]].
 
     - If the player command is ["go clock     tower"], then the object
       phrase is again [\["clock"; "tower"\]].
 
     An [object_phrase] is not permitted to be the empty list. *)

 
 (** The type [command] represents a player command that is decomposed
     into an action and a phrase. *)
 type command =
   | CreateDatabase of string (* Use: CREATE DATABASE my_database *)
   | DropDatabase of string (* Use: DROP DATABASE my_database *)
   | CreateTable of string (* Use: CREATE TABLE my_table *)
   | DropTable of string (* Use: DROP TABLE my_table *)
   (* Use: ALTER TABLE table_name ADD column_name int|float|bool|char|string; *)
   | AlterTable of alter_phrase 
   (* Use: SELECT col1, col2,... FROM table_name *)
   | Select of select_phrase
   (* Use: SELECT ALL FROM table_name *)
   | SelectAll of string
   (* INSERT INTO my_table (col1, col2,... ) (val1, val2,...) *)
   | InsertInto of insert_phrase 
   (* UPDATE table_name SET col1 = val1, col2 = val2,... WHERE condition
      Example: UPDATE Students SET Name = 'Joe', GPA = 3.5 WHERE Id = 1 *)
   | Update of update_phrase
   (* DELETE FROM table_name WHERE condition *)
   | Delete of delete_phrase
   | Quit
   (* TODO: double check that camelCase is correct here*)
 
 exception Empty
 (** Raised when an empty command is parsed. *)
 
 exception Malformed
 (** Raised when a malformed command is encountered. *)
 
 val parse : string -> command
 (** [parse str] parses a player's input into a [command], as follows.
     The first word (i.e., consecutive sequence of non-space characters)
     of [str] becomes the verb. The rest of the words, if any, become the
     object phrase. Examples:
 
     - [parse "    go   clock   tower   "] is [Go \["clock"; "tower"\]]
     - [parse "quit"] is [Quit].
 
     Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
     characters (only ASCII character code 32; not tabs or newlines,
     etc.).
 
     Raises: [Empty] if [str] is the empty string or contains only
     spaces.
 
     Raises: [Malformed] if the command is malformed. A command is
     {i malformed} if the verb is neither "quit" nor "go", or if the verb
     is "quit" and there is a non-empty object phrase, or if the verb is
     "go" and there is an empty object phrase.*)
 