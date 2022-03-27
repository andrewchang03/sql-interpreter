open Command

(** Representation of static table data.

    This module represents the data stored in csv files. It handles
    loading of that data as well as querying the data. *)

type t
(** The abstract type of values representing tables. *)

val create_table : string -> unit
(** [create_table f] is the name of the file that the empty table is
    created in. *)

val drop_table : Csv.t -> unit
(** [drop_table t] deletes the table [t]. *)

val alter_table_add :
  (string * Csv.t) list -> string -> string -> data_type -> Csv.t
(** [alter_table_add lst t col col_type] adds a column to table [t] in
    [lst] with name [col] and type [col_type]. *)

val alter_table_drop :
  (string * Csv.t) list -> string -> string -> data_type -> Csv.t
(** [alter_table_drop lst t col col_type] drops the column of table [t]
    in [lst] with name [col] and type [col_type]. Requires: [col] must
    exist in the table. Raises: Illegal exception if [col] is not in the
    table. *)

val alter_table_modify :
  (string * Csv.t) list -> string -> string -> data_type -> Csv.t
(** [alter_table_modify lst t col col_type] modifies the column of table
    [t] in [lst] with name [col] and type [col_type]. Requires: [col]
    must exist in the table. Raises: Illegal exception if [col] is not
    in the table. *)

val update :
  Csv.t -> string list -> string list -> ('a -> 'b -> bool) -> Csv.t
(** [update t s v c] is the updated table with the columns [s] updated
    to the values [v] where the condition [c] is true. *)

val insert : Csv.t -> string list -> string list -> Csv.t
(** [insert t c v] is the table [t] with a new row with values [v] in
    the respective columns [c] inserted at the beginning of the table*)
