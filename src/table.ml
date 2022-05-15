open Csv
open Parse

type t = string list list

(* transpose table to pop columns *)
let rec transpose_table (table : Csv.t) (n : int) : Csv.t =
  if n = List.length (List.nth table 0) then []
  else
    List.map (fun x -> List.nth x n) table
    :: transpose_table table (n + 1)

let rec select (table : Csv.t) (col_names : string list) : Csv.t =
  transpose_table
    (List.filter
       (fun x -> List.mem (List.nth x 0) col_names)
       (transpose_table table 0))
    0

(** [select_all tables name] finds table [name] in collection of tables
    [tables] and returns it. *)
let select_all (tables : (string * Csv.t) list) (name : string) =
  List.find (fun x -> fst x = name) tables |> snd

(* CREATE TABLE *)
let create_table
    (table_name : string)
    (col_names : (string * Parse.data_type) list) : Csv.t =
  if List.length col_names = 0 then []
  else
    let table =
      [
        List.map
          (fun col ->
            match col with
            | name, dt -> (
                name ^ ":"
                ^
                match dt with
                | BOOL -> "bool"
                | INT -> "int"
                | FLOAT -> "float"
                | STRING -> "string"
                | CHAR -> "char"
                | UNSUPPORTED s -> raise Malformed))
          col_names;
      ]
    in
    Csv.save ("data" ^ Filename.dir_sep ^ table_name ^ ".csv") table;
    table

(* DROP TABLE *)
let drop_table (tables : (string * Csv.t) list) (table_name : string) :
    (string * Csv.t) list =
  List.filter (fun x -> fst x <> table_name) tables

let insert
    (table_name : string)
    (cols : (string * data_type) list)
    (vals : string list) : Csv.t =
  let table =
    Csv.load ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
  in
  (* if List.nth table 0 <> cols then failwith "column names do not
     match" *)
  Csv.save
    ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
    (table @ [ vals ]);
  table @ [ vals ]

(* ALTER TABLE ADD *)
let rec add_empty_cols (data : string list list) : string list list =
  match data with
  | [] -> []
  | row :: tail -> ("nan" :: row) :: add_empty_cols tail

let is_duplicate_col (t : Csv.t) (col_names : string) : bool =
  match t with
  | columns :: t ->
      List.exists (fun col_name -> col_name = col_names) columns
  | [] -> raise NoTable

let alter_table_add_helper
    (t : Csv.t)
    (table_name : string)
    (col_name : string)
    (col_type : data_type) : string list list =
  let new_table =
    match t with
    | cols :: data ->
        if not (is_duplicate_col t col_name) then
          let type_name =
            match col_type with
            | INT -> "int"
            | BOOL -> "bool"
            | FLOAT -> "float"
            | STRING -> "string"
            | CHAR -> "char"
            | UNSUPPORTED s -> raise Malformed
          in
          ((col_name ^ ":" ^ type_name) :: cols) :: add_empty_cols data
        else raise (DuplicateName col_name)
    | [] -> raise NoTable
  in
  Csv.save ("data" ^ Filename.dir_sep ^ table_name ^ ".csv") new_table;
  new_table

let rec alter_add_in_place
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (col_type : data_type)
    (before : (string * Csv.t) list) : (string * Csv.t) list =
  match tables with
  | [] -> raise NoTable
  | (n, d) :: t when n = table_name ->
      before
      @ [ (n, alter_table_add_helper d table_name col_name col_type) ]
      @ t
  | h :: t ->
      alter_add_in_place t table_name col_name col_type (before @ [ h ])

let alter_table_add
    (tables : (string * Csv.t) list)
    (name : string)
    (col : string)
    (col_type : data_type) : (string * Csv.t) list =
  alter_add_in_place tables name col col_type []

(* ALTER TABLE DROP *)
let rec drop_cols (data : string list list) cols drop_col :
    string list list =
  match data with
  | [] -> []
  | row :: tail ->
      let assoc = List.combine cols row in
      let removed_assoc = List.remove_assoc drop_col assoc in
      snd (List.split removed_assoc) :: drop_cols tail cols drop_col

let alter_table_drop_helper
    (t : Csv.t)
    (table_name : string)
    (col_name : string)
    (col_type : data_type) : Csv.t =
  let new_table =
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
        List.filter (fun c -> c <> col_name ^ ":" ^ type_name) cols
        :: drop_cols t cols (col_name ^ ":" ^ type_name)
    | [] -> []
  in
  Csv.save ("data" ^ Filename.dir_sep ^ table_name ^ ".csv") new_table;
  new_table

let rec alter_drop_in_place
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (col_type : data_type)
    (before : (string * Csv.t) list) : (string * Csv.t) list =
  match tables with
  | [] -> raise NoTable
  | (n, d) :: t when n = table_name ->
      before
      @ [ (n, alter_table_drop_helper d table_name col_name col_type) ]
      @ t
  | h :: t ->
      alter_drop_in_place t table_name col_name col_type (before @ [ h ])

let alter_table_drop
    tables
    (table_name : string)
    (col_name : string)
    (col_type : data_type) : (string * Csv.t) list =
  alter_drop_in_place tables table_name col_name col_type []

(* ALTER TABLE MODIFY *)
let alter_table_modify
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (col_type : data_type) : (string * Csv.t) list =
  let new_table =
    let table =
      Csv.load ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
    in
    match table with
    | [] -> raise Malformed
    | h :: t ->
        let rec replace_col cols =
          match cols with
          | [] -> []
          | h' :: t' ->
              let type_name =
                match col_type with
                | INT -> "int"
                | BOOL -> "bool"
                | FLOAT -> "float"
                | STRING -> "string"
                | CHAR -> "char"
                | UNSUPPORTED s -> raise Malformed
              in
              if h' = col_name then type_name :: replace_col t'
              else h' :: replace_col t'
        in
        replace_col h :: t
  in
  (table_name, new_table)
  :: List.filter (fun x -> fst x <> table_name) tables

let rec get_row_indices
    (col : string list)
    (op : operator)
    (value : string)
    (index : int) : int list =
  match col with
  | [] -> []
  | h :: t ->
      if
        (op = LESS && h < value)
        || (op = LE && h <= value)
        || (op = GREATER && h > value)
        || (op = GE && h >= value)
        || (op = EQ && h = value)
      then index :: get_row_indices t op value (index + 1)
      else get_row_indices t op value (index + 1)

let rec where_find_col
    (transposed : Csv.t)
    (col_name : string)
    (op : operator)
    (value : string) : int list =
  match transposed with
  | [] -> []
  | h :: t ->
      if List.nth h 0 = col_name then get_row_indices h op value 0
      else where_find_col t col_name op value

let get_list_tail = function
  | [] -> []
  | h :: t -> t

let rec delete_table_helper
    (table : Csv.t)
    (indices : int list)
    (counter : int) : Csv.t =
  match table with
  | [] -> []
  | h :: t ->
      if List.length indices = 0 then table
      else if List.nth indices 0 = counter then
        delete_table_helper t (get_list_tail indices) (counter + 1)
      else h :: delete_table_helper t indices (counter + 1)

(* DELETE TABLE *)
let delete_table
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (op : operator)
    (value : string) : (string * Csv.t) list =
  let table =
    Csv.load ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
  in
  let delete_rows =
    where_find_col (transpose_table table 0) col_name op value
  in
  Csv.save
    ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
    (delete_table_helper table delete_rows 0);
  (table_name, delete_table_helper table delete_rows 0)
  :: List.filter (fun x -> fst x <> table_name) tables

(* SELECT FROM WHERE *)

let rec select_where_helper
    (table : Csv.t)
    (indices : int list)
    (counter : int) : Csv.t =
  match table with
  | [] -> []
  | h :: t ->
      if counter = 0 then
        h :: select_where_helper t indices (counter + 1)
      else if List.length indices = 0 then []
      else if List.nth indices 0 = counter then
        h :: select_where_helper t (get_list_tail indices) (counter + 1)
      else select_where_helper t indices (counter + 1)

let select_where_table
    (* (tables : (string * Csv.t) list) *)
      (table : Csv.t)
    (col_name : string)
    (op : operator)
    (value : string) : Csv.t =
  let select_rows =
    where_find_col (transpose_table table 0) col_name op value
  in
  select_where_helper table select_rows 0

(* UPDATE TABLE *)

let rec update_helper
    (table : Csv.t)
    (cols : string list)
    (vals : string list)
    (indices : int list)
    (counter : int) : Csv.t =
  match table with
  | [] -> []
  | h :: t ->
      if counter = 0 then
        h :: update_helper t cols vals indices (counter + 1)
      else if List.length indices = 0 then table
      else if List.nth indices 0 = counter then
        vals
        :: update_helper t cols vals (get_list_tail indices)
             (counter + 1)
      else h :: update_helper t cols vals indices (counter + 1)

let update_table
    (table_name : string)
    (cols : string list)
    (vals : string list)
    (col_name : string)
    (op : operator)
    (value : string) : Csv.t =
  let table =
    Csv.load ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
  in
  let update_rows =
    where_find_col (transpose_table table 0) col_name op value
  in
  Csv.save
    ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
    (update_helper table cols vals update_rows 0);
  update_helper table cols vals update_rows 0
