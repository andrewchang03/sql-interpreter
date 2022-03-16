open Csv

type t = string list list

let create_table fname =
  Csv.save ("data" ^ Filename.dir_sep ^ fname ^ ".csv") []

let drop_table t =
  raise (Stdlib.Failure "Unimplemented: Table.drop_table")

let update t col vals cond =
  raise (Stdlib.Failure "Unimplemented: Table.update_table")

let insert t col vals =
  raise (Stdlib.Failure "Unimplemented: Table.insert")

let swap_rows t =
  raise (Stdlib.Failure "Unimplemented: Table.drop_table")

let swap_cols t =
  raise (Stdlib.Failure "Unimplemented: Table.drop_table")
