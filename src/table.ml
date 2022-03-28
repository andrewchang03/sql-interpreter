open Csv
open Command

type t = string list list

(* SELECT *)
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

let select_all tables name =
  List.find (fun x -> fst x = name) tables |> snd

let create_table fname (cols : (string * Command.data_type) list) =
  let table =
    [
      List.map
        (fun col ->
          match col with
          | name, dt -> (
              name ^ " "
              ^
              match dt with
              | BOOL -> "bool"
              | INT -> "int"
              | FLOAT -> "float"
              | STRING -> "string"
              | CHAR -> "char"
              | UNSUPPORTED s -> raise Malformed))
        cols;
    ]
  in
  Csv.save ("data" ^ Filename.dir_sep ^ fname ^ ".csv") table;
  table

let drop_table tables name = List.filter (fun x -> fst x <> name) tables

let update t col vals cond =
  raise (Stdlib.Failure "Unimplemented: Table.update_table")

let load_table fname =
  Csv.load ("data" ^ Filename.dir_sep ^ fname ^ ".csv")

let alter_table_add lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_add")

let alter_table_drop lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_drop")

let alter_table_modify lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_modify")

(* INSERT INTO *)
let insert (fname : string) (cols : string list) (vals : string list) =
  if List.length cols = List.length vals then
    Csv.save
      ("data" ^ Filename.dir_sep ^ fname ^ ".csv")
      (load_table fname @ [ vals ])
  else raise (Stdlib.Failure "Columns do not match values")

let swap_rows t =
  raise (Stdlib.Failure "Unimplemented: Table.drop_table")

let swap_cols t =
  raise (Stdlib.Failure "Unimplemented: Table.drop_table")

(* let get_first_el lis = match lis with | [] -> [] | h :: t -> h

   let remove_one_el lis = match lis with | [] -> [] | h :: t -> t

   let rec update_helper acc col vals cond = match vals with | [] -> acc
   | h :: t -> if cond h then update_helper (h @ acc) (remove_one_el
   col) t cond else update_helper (get_first_el col @ acc)
   (remove_one_el vals) t cond

   let rec new_table acc (temp : 'a list) (col : 'a list) t = match t
   with | [] -> [] | h :: t -> if h = col then new_table (acc @ [ temp
   ]) temp col t else new_table (acc @ [ h ]) temp col t

   let update t (col : 'a list) vals cond = let temp = update_helper []
   col vals cond in new_table [] temp col t *)
