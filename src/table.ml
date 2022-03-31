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

(* CREATE TABLE *)
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

(* DROP TABLE *)
let drop_table tables name = List.filter (fun x -> fst x <> name) tables

(* UPDATE TABLE *)

let get_first_el (lis : string list) : string =
  match lis with
  | [] -> ""
  | h :: t -> h

let remove_one_el lis =
  match lis with
  | [] -> []
  | h :: t -> t

let rec update_helper acc (col : string list) (vals : 'a list) cond =
  match vals with
  | [] -> acc
  | h :: t ->
      if cond h then update_helper (h :: acc) (remove_one_el col) t cond
      else
        update_helper
          (get_first_el col :: acc)
          (remove_one_el vals) t cond

let rec new_table acc (temp : 'a list) (col : 'a list) t =
  match t with
  | [] -> []
  | h :: t ->
      if h = col then new_table (acc @ [ temp ]) temp col t
      else new_table (acc @ [ h ]) temp col t

let update
    (t : Csv.t)
    (col : string list)
    (vals : 'a list)
    (cond : 'a -> bool) =
  let temp = update_helper [] col vals cond in
  new_table [] temp col t

let update t cols vals cond =
  raise (Stdlib.Failure "Unimplemented: update")

let load_table fname =
  Csv.load ("data" ^ Filename.dir_sep ^ fname ^ ".csv")

let rec get_table (tables : (string * Csv.t) list) name =
  match tables with
  | [] -> []
  | (n, d) :: t when n = name -> d
  | _ :: t -> get_table t name

(* ALTER TABLE ADD *)
let rec add_empty_cols (data : string list list) =
  match data with
  | [] -> []
  | row :: tail -> ("" :: row) :: add_empty_cols tail

let is_duplicate_col (t : Csv.t) col =
  match t with
  | cols :: t -> List.exists (fun col_name -> col_name = col) cols
  | [] -> raise NoTable

let alter_table_add_helper (t : Csv.t) name col col_type =
  let result =
    match t with
    | cols :: data ->
        if not (is_duplicate_col t col) then
          let type_name =
            match col_type with
            | INT -> "int"
            | BOOL -> "bool"
            | FLOAT -> "float"
            | STRING -> "string"
            | CHAR -> "char"
            | UNSUPPORTED s -> raise Malformed
          in
          ((col ^ " " ^ type_name) :: cols) :: add_empty_cols data
        else raise (DuplicateName col)
    | [] -> raise NoTable
  in
  Csv.save ("data" ^ Filename.dir_sep ^ name ^ ".csv") result;
  result

let rec alter_add_in_place tables name col col_type before =
  match tables with
  | [] -> raise NoTable
  | (n, d) :: t when n = name ->
      before @ [ (n, alter_table_add_helper d name col col_type) ] @ t
  | h :: t -> alter_add_in_place t name col col_type (before @ [ h ])

let alter_table_add tables name col col_type =
  alter_add_in_place tables name col col_type []

(* ALTER TABLE DROP *)
let rec drop_cols (data : string list list) =
  match data with
  | [] -> []
  | row :: tail -> ("" :: row) :: drop_cols tail

let alter_table_drop_helper (t : Csv.t) name col col_type =
  let result =
    match t with
    | cols :: t ->
        let type_name =
          match col_type with
          | INT -> "int"
          | BOOL -> "bool"
          | FLOAT -> "float"
          | STRING -> "string"
          | CHAR -> "char"
          | UNSUPPORTED s -> raise Malformed
        in
        List.filter (fun c -> c <> col ^ " " ^ type_name) cols
        :: drop_cols t
    | [] -> []
  in
  Csv.save ("data" ^ Filename.dir_sep ^ name ^ ".csv") result;
  result

let rec alter_drop_in_place tables name col col_type before =
  match tables with
  | [] -> raise NoTable
  | (n, d) :: t when n = name ->
      before @ [ (n, alter_table_drop_helper d name col col_type) ] @ t
  | h :: t -> alter_drop_in_place t name col col_type (before @ [ h ])

let alter_table_drop
    tables
    (name : string)
    (col : string)
    (col_type : data_type) =
  alter_drop_in_place tables name col col_type []

let alter_table_modify tables name col col_type =
  raise (Stdlib.Failure "Unimplemented: Table.alter_table_modify")

let rec match_col_vals cols vals =
  match vals with
  | [] -> []
  | h :: val_t -> (
      match cols with
      | [] -> []
      | (name, dt) :: col_t ->
          (name, dt, h) :: match_col_vals col_t val_t)

let rec check_valid lst =
  match lst with
  | [] -> ()
  | (name, dt, h) :: t -> (
      match dt with
      | BOOL -> (
          match bool_of_string_opt h with
          | Some _ -> check_valid t
          | None -> raise Malformed)
      | INT -> (
          match int_of_string_opt h with
          | Some _ -> check_valid t
          | None -> raise Malformed)
      | FLOAT -> (
          match float_of_string_opt h with
          | Some _ -> check_valid t
          | None -> raise Malformed)
      | STRING -> check_valid t
      | CHAR ->
          if String.length h = 1 then check_valid t
          else raise (Stdlib.Failure h)
      | UNSUPPORTED s -> raise Malformed)

let insert
    (fname : string)
    (cols : (string * Command.data_type) list)
    (vals : string list) =
  if List.length cols = List.length vals then
    try
      check_valid (match_col_vals cols vals);
      let table = load_table fname @ [ vals ] in
      Csv.save ("data" ^ Filename.dir_sep ^ fname ^ ".csv") table;
      table
    with
    | Invalid_argument s ->
        raise (Stdlib.Failure "Columns do\n       not match values")
    | Stdlib.Failure s ->
        raise (Stdlib.Failure "Columns do not match values")
    | Malformed -> raise (Stdlib.Failure "Columns do not match values")
  else raise (Stdlib.Failure "Columns do not match values")

(* let swap_rows t = raise (Stdlib.Failure "Unimplemented:
   Table.drop_table")

   let swap_cols t = raise (Stdlib.Failure "Unimplemented:
   Table.drop_table") *)

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
