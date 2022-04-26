open Csv
open Command

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

let select table (col_names : string list) =
  transpose_table
    (List.filter
       (fun x -> List.mem (List.nth x 0) col_names)
       (transpose_table table 0))
    0

let select_all tables name =
  List.find (fun x -> fst x = name) tables |> snd

let create_table fname =
  Csv.save ("data" ^ Filename.dir_sep ^ fname ^ ".csv") []

let drop_table tables name = List.filter (fun x -> fst x <> name) tables

let get_first_el lis =
  match lis with
  | [] -> []
  | h :: t -> h

let remove_one_el lis =
  match lis with
  | [] -> []
  | h :: t -> t

let load_table fname =
  Csv.load ("data" ^ Filename.dir_sep ^ fname ^ ".csv")

let alter_table_add lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_add")

let alter_table_drop lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_drop")

let alter_table_modify lst t col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_modify")

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

let rec new_table acc (temp : 'a list) (col : 'a list) t =
  match t with
  | [] -> []
  | h :: t ->
      if h = col then new_table (acc @ [ temp ]) temp col t
      else new_table (acc @ [ h ]) temp col t

let rec get_row tab num count =
  match tab with
  | [] -> []
  | h :: t -> if num = count then h else get_row t num (count + 1)


let rec is_in item col =
    (**START*)
  match col with
  | [] -> None
  | h :: t -> if h = item then Some item else is_in item t

let check col ind acc =
  for each=0 to (length col)
    let temp = is_in each ind in
    if temp <> None then
      match temp with
      | None -> item :: acc
      | Some v -> v :: acc

let rec help2 header col acc count =
  match col with
  | [] -> acc
  | h :: t -> if (is_in h col) <> None then help2 header col (h::acc) (count+1)
  else help2 header col (acc) (count+1)
let update t cols vals cond =
  acc = []
indicies = help2 (get_row t 0 0) col [] 0
for x = 0 to (length t) do
  if cond (get_row x 0) then check col indicies []

let load_table fname =
  Csv.load ("data" ^ Filename.dir_sep ^ fname ^ ".csv")

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
