open Csv

type t = string list list

let create_table t =
  raise (Stdlib.Failure "Unimplemented: Table.create_table")

let drop_table t =
  raise (Stdlib.Failure "Unimplemented: Table.drop_table")

let update t col vals cond =
  raise (Stdlib.Failure "Unimplemented: Table.update_table")

let insert t col vals =
  raise (Stdlib.Failure "Unimplemented: Table.insert")

(* let data = load "data/students.csv" *)
