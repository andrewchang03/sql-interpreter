open Parse

(** Representation of static table data along with functions that act on
    tables. *)

type t
(** type [t] is the abstract type of values representing tables. *)

val transpose_table : Csv.t -> int -> Csv.t
(** [transpose_table table 0] transposes tabular list form of [table] *)

val select : Csv.t -> string list -> Csv.t
(** [select table col_names] grabs columns in col_names from the table *)

val select_where_table :
  (* (string * Csv.t) list -> *)
  Csv.t ->
  string ->
  operator ->
  string ->
  Csv.t
(** [select_where_table t c op v] grabs table entries that satisfy the
    condition *)

val select_all : (string * Csv.t) list -> string -> Csv.t
(** [select_all tables table_name] returns the whole queried table *)

val create_table : string -> (string * Parse.data_type) list -> Csv.t
(** [create_table table_name cols] creates a new .csv file with name
    [table_name] populated with [cols], which describes the name of each
    column. Returns: the newly created table. *)

val drop_table :
  (string * Csv.t) list -> string -> (string * Csv.t) list
(** [drop_table tables name] deletes the table with [name] from
    [tables]. *)

val insert :
  string -> (string * Parse.data_type) list -> string list -> Csv.t
(** [insert t c v] is the table [t] with a new row with values [v] in
    the respective columns [c] inserted at the beginning of the table.
    Precondition: every row is input in order, missing values nan. *)

val alter_table_add :
  (string * Csv.t) list ->
  string ->
  string ->
  data_type ->
  (string * Csv.t) list
(** [alter_table_add lst t col col_type] adds a column to table [t] in
    [lst] with name [col] and type [col_type]. *)

val alter_table_drop :
  (string * Csv.t) list ->
  string ->
  string ->
  data_type ->
  (string * Csv.t) list
(** [alter_table_drop lst t col col_type] drops the column of table [t]
    in [lst] with name [col] and type [col_type]. Requires: [col] must
    exist in the table. Raises: Illegal exception if [col] is not in the
    table. *)

val alter_table_modify :
  (string * Csv.t) list ->
  string ->
  string ->
  data_type ->
  (string * Csv.t) list
(** [alter_table_modify lst t col col_type] modifies the column of table
    [t] in [lst] with name [col] and type [col_type]. Requires: [col]
    must exist in the table. Raises: Illegal exception if [col] is not
    in the table. *)

val delete_table :
  (string * Csv.t) list ->
  string ->
  string ->
  operator ->
  string ->
  (string * Csv.t) list
(** [delete_table t table_name col_name op value] deletes rows in the
    table [table_name] that specifies the condition provided by
    [col_name], [op], [value] *)

val update_table :
  string ->
  string list ->
  string list ->
  string ->
  operator ->
  string ->
  Csv.t
(** [update_table table_name cols vals left op right] updates the
    entries in [table_name] where condition satisfies. *)

val aggregate_int_columns :
  (string * Csv.t) list -> string -> string -> aggregate_int -> int
(** [aggregate_int_columns tables table_name col_name op] finds
    according integer column in a table and performs integer accumulator
    operations on it. *)

val aggregate_string_columns :
  (string * Csv.t) list ->
  string ->
  string ->
  aggregate_string ->
  string
(** [aggregate_string_columns tables table_name col_name op] finds
    according string column in a table and performs string accumulator
    operations on it. *)

val aggregate_boolean_columns :
  (string * Csv.t) list ->
  string ->
  string ->
  aggregate_boolean ->
  string
(** [aggregate_boolean_columns tables table_name col_name op] finds
    according boolean column in a table and performs boolean accumulator
    operations on it. *)
