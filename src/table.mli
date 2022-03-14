open Command

(** Representation of static table data.

    This module represents the data stored in csv files. It handles
    loading of that data as well as querying the data. *)

type t
(** The abstract type of values representing tables. *)

val create_table : string -> unit
(** [create_table f] is the name of the file that the empty table is created in. *)

val drop_table : Csv.t -> unit
(** [drop_table t] deletes the table [t]. *)

val update :
  Csv.t -> string list -> string list -> ('a -> 'b -> bool) -> Csv.t
(** [update t s v c] is the updated table with the columns [s] updated
    to the values [v] where the condition [c] is true. *)

val insert : Csv.t -> string list -> string list -> Csv.t
(** [insert t c v] is the table [t] with a new row with values [v] in
    the respective columns [c] inserted at the beginning of the table*)
