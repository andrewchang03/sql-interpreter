open Command

(** Representation of static table data along with functions that act on
    tables. *)

type t
(** type [t] is the abstract type of values representing tables. *)

val select : Csv.t -> string list -> Csv.t
(** [select table col_names] grabs columns in col_names from the table *)

val select_all : (string * Csv.t) list -> string -> Csv.t
(** [select_all tables table_name] returns the whole queried table *)

val create_table : string -> (string * Command.data_type) list -> Csv.t
(** [create_table table_name cols] creates a new .csv file with name
    [table_name] populated with [cols], which describes the name and
    data type of each column. Returns: the newly created table. *)

val drop_table : ('a * 'b) list -> 'a -> ('a * 'b) list
(** [drop_table tables name] deletes the table with [name] from
    [tables]. *)

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

val update :
  Csv.t -> string list -> string list -> ('a -> bool) -> Csv.t
(** [update t s v c] is the updated table with the columns [s] updated
    to the values [v] where the condition [c] is true. *)

val insert :
  string -> (string * Command.data_type) list -> string list -> Csv.t
(** [insert t c v] is the table [t] with a new row with values [v] in
    the respective columns [c] inserted at the beginning of the table*)