open Csv
open Command

type t = string list list

let create_table fname =
  Csv.save ("data" ^ Filename.dir_sep ^ fname ^ ".csv") []

let drop_table t =
  raise (Stdlib.Failure "Unimplemented: Table.drop_table")

let update t col vals cond =
  raise (Stdlib.Failure "Unimplemented: Table.update_table")

let insert t col vals =
  raise (Stdlib.Failure "Unimplemented: Table.insert")

let alter_table_add lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_add")

let alter_table_drop lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_drop")

let alter_table_modify lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_modify")
