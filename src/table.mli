open Command

(** Representation of static table data. This module represents the data
    stored in csv files. It handles loading of that data as well as
    querying the data. *)

type t
(** The abstract type of values representing tables. *)

val select : Csv.t -> string list -> Csv.t
(** [select table col_names] grab columns in col_names from the table *)

<<<<<<< HEAD
val select_all : (string * Csv.t) list -> string -> Csv.t
(** [select_all tables table_name] returns the whole queried table *)

val create_table : string -> (string * Command.data_type) list -> Csv.t
(** [create_table table_name cols] creates a new .csv file with name
    [table_name] populated with [cols], which describes the name and
    data type of each column. Returns: the newly created table. *)
=======
val select_all : ('a * 'b) list -> 'a -> 'b

val create_table : string -> unit
(** [create_table f] is the name of the file that the empty table is
    created in. *)
>>>>>>> cce7187 (I added the update function with helpers)

val drop_table : ('a * 'b) list -> 'a -> ('a * 'b) list
(** [drop_table tables name] deletes the table with [name] from
    [tables]. *)
<<<<<<< HEAD

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
=======
>>>>>>> cce7187 (I added the update function with helpers)

val update :
  Csv.t -> string list -> string list -> ('a -> 'b -> bool) -> Csv.t
(** [update t s v c] is the updated table with the columns [s] updated
    to the values [v] where the condition [c] is true. *)

val insert : string -> string list -> string list -> unit
(** [insert t c v] is the table [t] with a new row with values [v] in
    the respective columns [c] inserted at the beginning of the table*)

val swap_rows : t -> string list -> string list -> Csv.t
(** [swap_rows t r1 r2 v] is the table [t] with the first row of values
    [r1] with the second rows [r2] in the respective columns and returns
    the new table *)

val swap_cols : t -> string list -> string list -> Csv.t
(** [swap_cols t r1 r2 v] is the table [t] with the first col of values
    [c1] with the second cols [c2] in the respective columns and returns
    the new table *)