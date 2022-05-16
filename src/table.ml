open Csv
open Parse

type t = string list list

(* transpose table to pop columns *)
let rec transpose_table (table : Csv.t) (n : int) : Csv.t =
  if n = List.length (List.nth table 0) then []
  else
    List.map (fun x -> List.nth x n) table
    :: transpose_table table (n + 1)

(* SELECT *)
let rec select (table : Csv.t) (col_names : string list) : Csv.t =
  transpose_table
    (List.filter
       (fun x -> List.mem (List.nth x 0) col_names)
       (transpose_table table 0))
    0

(* SELECT ALL *)
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

(* [match_col_vals cols vals] is a tuple that matches columns and
   datatypes to values in list *)
let rec match_col_vals cols vals =
  match vals with
  | [] -> []
  | h :: val_t -> (
      match cols with
      | [] -> []
      | (name, dt) :: col_t ->
          (name, dt, h) :: match_col_vals col_t val_t)

(* [check_valid lst] is lst if the list values match the datatype of the
   column and raises an invalid_arg if it does not *)
let rec check_valid lst =
  match lst with
  | [] -> ()
  | (name, dt, h) :: t -> (
      match dt with
      | BOOL -> (
          match bool_of_string_opt h with
          | Some _ -> check_valid t
          | None -> invalid_arg "not a bool")
      | INT -> (
          match int_of_string_opt h with
          | Some i -> check_valid t
          | None -> invalid_arg "not an int")
      | FLOAT -> (
          match float_of_string_opt h with
          | Some _ -> check_valid t
          | None -> invalid_arg "not a float")
      | STRING -> check_valid t
      | CHAR ->
          if String.length h = 1 then check_valid t
          else invalid_arg "not a char"
      | UNSUPPORTED s -> invalid_arg "unsupported")

(*[load_table fname] loads the csv table from file [fname] in the data
  folder *)
let load_table fname =
  Csv.load ("data" ^ Filename.dir_sep ^ fname ^ ".csv")

(*INSERT*)
let insert
    (table_name : string)
    (cols : (string * data_type) list)
    (vals : string list) : Csv.t =
  if List.length cols = List.length vals then
    try
      check_valid (match_col_vals cols vals);
      let table = load_table table_name @ [ vals ] in
      Csv.save ("data" ^ Filename.dir_sep ^ table_name ^ ".csv") table;
      table
    with
    | Invalid_argument s ->
        raise (Stdlib.Failure "Columns do not match values")
    | Stdlib.Failure s ->
        raise (Stdlib.Failure "Columns do not match values")
    | Malformed -> raise (Stdlib.Failure "Columns do not match values")
  else raise (Stdlib.Failure "Columns do not match values")

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

let rec alter_table_add
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (col_type : data_type) : (string * Csv.t) list =
  match tables with
  | [] -> raise NoTable
  | (n, d) :: t when n = table_name ->
      (n, alter_table_add_helper d table_name col_name col_type) :: t
  | h :: t -> h :: alter_table_add t table_name col_name col_type

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

let rec alter_table_drop
    tables
    (table_name : string)
    (col_name : string)
    (col_type : data_type) : (string * Csv.t) list =
  match tables with
  | [] -> raise NoTable
  | (n, d) :: t when n = table_name ->
      (n, alter_table_drop_helper d table_name col_name col_type) :: t
  | h :: t -> h :: alter_table_drop t table_name col_name col_type

(* ALTER TABLE MODIFY *)
let rec replace_col cols col_name col_type =
  match cols with
  | [] -> []
  | h' :: t' ->
      let c = List.nth (String.split_on_char ':' h') 0 in
      if c = col_name then
        let type_name =
          match col_type with
          | INT -> "int"
          | BOOL -> "bool"
          | FLOAT -> "float"
          | STRING -> "string"
          | CHAR -> "char"
          | UNSUPPORTED s -> raise Malformed
        in
        (c ^ ":" ^ type_name) :: t'
      else h' :: replace_col t' col_name col_type

let rec alter_table_modify
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (col_type : data_type) : (string * Csv.t) list =
  match tables with
  | [] -> raise NoTable
  | (n, d) :: t when n = table_name ->
      let new_table =
        match d with
        | [] -> raise Malformed
        | h :: t -> replace_col h col_name col_type :: t
      in
      (n, new_table) :: t
  | h :: t -> h :: alter_table_modify t table_name col_name col_type

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

let remove_first (table : 'a list) : 'a list =
  match table with
  | h :: t -> t
  | [] -> []

let rec where_find_col
    (transposed : Csv.t)
    (col_name : string)
    (op : operator)
    (value : string) : int list =
  match transposed with
  | [] -> []
  | h :: t ->
      if List.nth h 0 = col_name then
        get_row_indices (remove_first h) op value 0
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
  let table = load_table table_name in
  let body = remove_first table in
  let delete_rows =
    where_find_col (transpose_table table 0) col_name op value
  in
  Csv.save
    ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
    (List.nth table 0 :: delete_table_helper body delete_rows 0);
  ( table_name,
    List.nth table 0 :: delete_table_helper body delete_rows 0 )
  :: List.filter (fun x -> fst x <> table_name) tables

(* SELECT FROM WHERE *)
let rec select_where_helper
    (table : Csv.t)
    (indices : int list)
    (counter : int) : Csv.t =
  match table with
  | [] -> []
  | h :: t ->
      if List.length indices = 0 then []
      else if List.nth indices 0 = counter then
        h :: select_where_helper t (get_list_tail indices) (counter + 1)
      else select_where_helper t indices (counter + 1)

let select_where_table
    (table : Csv.t)
    (col_name : string)
    (op : operator)
    (value : string) : Csv.t =
  let body = remove_first table in
  let select_rows =
    where_find_col (transpose_table table 0) col_name op value
  in
  List.nth table 0 :: select_where_helper body select_rows 0

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
  let table = load_table table_name in
  let body = remove_first table in
  let update_rows =
    where_find_col (transpose_table table 0) col_name op value
  in
  Csv.save
    ("data" ^ Filename.dir_sep ^ table_name ^ ".csv")
    (List.nth table 0 :: update_helper body cols vals update_rows 0);
  List.nth table 0 :: update_helper body cols vals update_rows 0

let aggregate_int_columns
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (op : aggregate_int) : int =
  let table = List.find (fun x -> fst x = table_name) tables |> snd in
  let transposed = transpose_table table 0 in
  let filtered =
    List.filter (fun x -> List.nth x 0 = col_name) transposed
    |> List.flatten
  in
  let mapped =
    match filtered with
    | [] -> failwith "Something is wrong with your table."
    | h :: t -> List.map (fun x -> int_of_string x) t
  in
  match op with
  | AVERAGE -> List.fold_left ( + ) 0 mapped / List.length mapped
  | MEDIAN ->
      if List.length mapped mod 2 = 0 then
        List.nth
          (List.sort (fun x y -> x - y) mapped)
          (List.length mapped / 2)
      else
        List.nth
          (List.sort (fun x y -> x - y) mapped)
          (List.length mapped / 2)
        + List.nth
            (List.sort (fun x y -> x - y) mapped)
            ((List.length mapped / 2) + 1)
        |> ( / ) 2
  | SUM -> List.fold_left ( + ) 0 mapped
  | PRODUCT -> List.fold_left ( * ) 1 mapped
  | MIN -> List.nth (List.sort (fun x y -> x - y) mapped) 0
  | MAX ->
      List.nth
        (List.sort (fun x y -> x - y) mapped)
        (List.length mapped - 1)
  | COUNT -> List.length mapped

let aggregate_string_columns
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (op : aggregate_string) : string =
  let table = List.find (fun x -> fst x = table_name) tables |> snd in
  let transposed = transpose_table table 0 in
  let filtered =
    List.filter (fun x -> List.nth x 0 = col_name) transposed
    |> List.flatten
  in
  let no_head =
    match filtered with
    | [] -> failwith "Something is wrong with your table."
    | h :: t -> t
  in
  match op with
  | CONCAT -> String.concat ", " no_head
  | CHARACTER_COUNT ->
      string_of_int
        (List.fold_left ( + ) 0
           (List.map (fun x -> String.length x) no_head))
  | WORD_COUNT -> string_of_int (List.length no_head)

let aggregate_boolean_columns
    (tables : (string * Csv.t) list)
    (table_name : string)
    (col_name : string)
    (op : aggregate_boolean) : string =
  let table = List.find (fun x -> fst x = table_name) tables |> snd in
  let transposed = transpose_table table 0 in
  let filtered =
    List.filter (fun x -> List.nth x 0 = col_name) transposed
    |> List.flatten
  in
  let mapped =
    match filtered with
    | [] -> failwith "Something is wrong with your table."
    | h :: t -> List.map (fun x -> bool_of_string x) t
  in
  match op with
  | AND -> string_of_bool (List.fold_left ( && ) true mapped)
  | OR -> string_of_bool (List.fold_left ( || ) false mapped)
  | NAND ->
      string_of_bool
        (List.fold_left (fun x y -> not (x && y)) true mapped)
  | NOR ->
      string_of_bool
        (List.fold_left (fun x y -> not (x || y)) false mapped)
  | XOR ->
      string_of_bool
        (List.fold_left
           (fun x y -> (not (x && y)) || (x && y) |> not)
           true mapped)
