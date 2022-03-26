open Csv

type t = string list list

let rec transpose_table (table : Csv.t) (n : int) : Csv.t =
  if n = List.length (List.nth table 0) then []
  else
    List.map (fun x -> List.nth x n) table
    :: transpose_table table (n + 1)

let select table (col_names : string list) =
  transpose_table
    (List.filter
       (fun x -> List.mem (List.nth x 0) col_names)
       (transpose_table table 0))
    0

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
