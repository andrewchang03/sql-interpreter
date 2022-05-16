open Table
open Parse

type direction =
  | LEFT
  | RIGHT
  | UP
  | DOWN

type action =
  | Move of direction
  | Enter

type day_selector = {
  x : float;
  mutable y : float;
}

type level_selector = {
  x : float;
  mutable y : float;
}

type name_selector = {
  x : float;
  mutable y : float;
}

type state = {
  mutable day : day_selector;
  mutable level : level_selector;
  mutable name : name_selector;
  mutable select_pos : int;
  mutable table : Csv.t;
}

let init_days : day_selector = { x = 100.; y = 600. }
let init_level : level_selector = { x = 300.; y = 550. }
let init_name : name_selector = { x = 500.; y = 600. }
let font = Glut.BITMAP_HELVETICA_18

let drawString ?(font = font) x y s =
  GlMat.load_identity ();
  GlPix.raster_pos ~x ~y ();
  String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c)) s

let render_box x y color =
  GlMat.load_identity ();
  GlMat.translate3 (x, y, 0.);
  GlDraw.color color;
  GlDraw.begins `line_loop;
  List.iter GlDraw.vertex2
    [ (-10., -10.); (-10., 20.); (140., 20.); (140., -10.) ];
  GlDraw.color (255., 255., 255.);
  GlDraw.ends ()

let rec_row y color row =
  match row with
  | [] -> ()
  | [ col1; col2; col3 ] ->
      drawString 80. y col1;
      drawString (80. +. 80.) y col2;
      drawString (80. +. 500.) y col3;
      GlMat.translate3 (80., y -. 20., 0.);
      GlDraw.color color;
      GlDraw.begins `line_loop;
      List.iter GlDraw.vertex2
        [ (-10., 50.); (-10., 0.); (650., 0.); (650., 50.) ];
      GlDraw.ends ()
  | _ -> failwith "csClasses.csv contains invalid row"

let rec render_rows pos y color rows =
  match rows with
  | [] -> ()
  | h :: t ->
      if pos <= 8. then (
        rec_row y color h;
        render_rows (pos +. 1.) (400. -. (50. *. pos)) color t)
      else ()

let render_table color rows =
  match List.nth_opt rows 0 with
  | Some col ->
      let num_rows = List.length rows |> float_of_int in
      render_rows 1. 400. color rows
  | None -> ()

let controller state action =
  match action with
  | Move direction -> begin
      match direction with
      | LEFT ->
          if state.select_pos = 1 then ()
          else if state.select_pos = 2 then state.select_pos <- 1
          else if state.select_pos = 3 then state.select_pos <- 2
      | RIGHT ->
          if state.select_pos = 3 then ()
          else if state.select_pos = 2 then state.select_pos <- 3
          else if state.select_pos = 1 then state.select_pos <- 2
      | UP ->
          if state.select_pos = 1 && state.day.y > 500. then
            state.day.y <- state.day.y -. 50.
          else if state.select_pos = 2 && state.level.y > 500. then
            state.level.y <- state.level.y -. 50.
          else if state.select_pos = 3 && state.name.y > 500. then
            state.name.y <- state.name.y -. 50.
      | DOWN ->
          if state.select_pos = 1 && state.day.y < 600. then
            state.day.y <- state.day.y +. 50.
          else if state.select_pos = 2 && state.level.y < 600. then
            state.level.y <- state.level.y +. 50.
          else if state.select_pos = 3 && state.name.y < 600. then
            state.name.y <- state.name.y +. 50.
    end
  | Enter ->
      let day_query =
        let table =
          Csv.load ("data" ^ Filename.dir_sep ^ "csClasses" ^ ".csv")
        in
        if state.day.y = 500. then
          Table.select_where_table table "time:string" EQ "Other"
        else if state.day.y = 550. then
          Table.select_where_table table "time:string" EQ "Tues/Thurs"
        else
          Table.select_where_table table "time:string" EQ "Mon/Wed/Fri"
      in
      let level_query =
        if state.level.y = 500. then
          Table.select_where_table
            (Table.select_where_table day_query "id:int" GE "5000")
            "id:int" LESS "7000"
        else if state.level.y = 550. then
          Table.select_where_table
            (Table.select_where_table day_query "id:int" GE "3000")
            "id:int" LESS "5000"
        else
          Table.select_where_table
            (Table.select_where_table day_query "id:int" GE "1000")
            "id:int" LESS "3000"
      in
      (* let name_query = if state.name.y = 500. then
         Table.select_where_table level_query "name:string" GREATER "Q"
         else if state.level.y = 550. then Table.select_where_table
         (Table.select_where_table level_query "name:string" GREATER
         "G") "name:string" LESS "P" else Table.select_where_table
         level_query "name:string" LESS "G" in *)
      state.table <- level_query

let init () =
  {
    day = init_days;
    level = init_level;
    name = init_name;
    select_pos = 1;
    table = Csv.load ("data" ^ Filename.dir_sep ^ "csClasses" ^ ".csv");
  }

let render (state : state) =
  GlClear.clear [ `color ];
  drawString 100. 730.
    "1. Use arrow keys to select your search options.";
  drawString 100. 700. "2. Press the enter key to submit the query.";
  drawString ~font:Glut.BITMAP_TIMES_ROMAN_24 100. 650. "Days Offered";
  drawString ~font:Glut.BITMAP_TIMES_ROMAN_24 300. 650. "Course Level";
  drawString ~font:Glut.BITMAP_TIMES_ROMAN_24 500. 650. "Course Name";
  drawString 100. 600. "Mon/Wed/Fri";
  drawString 100. 550. "Tues/Thurs";
  drawString 100. 500. "Other";
  drawString 300. 600. "1000-2999";
  drawString 300. 550. "3000-4999";
  drawString 300. 500. "5000-7999";
  drawString 500. 600. "A-F";
  drawString 500. 550. "G-P";
  drawString 500. 500. "Q-Z";
  let white = (255., 255., 255.) in
  let green = (0., 255., 0.) in
  render_table white state.table;
  if state.select_pos == 1 then render_box state.day.x state.day.y green
  else render_box state.day.x state.day.y white;
  if state.select_pos == 2 then
    render_box state.level.x state.level.y green
  else render_box state.level.x state.level.y white;
  if state.select_pos == 3 then
    render_box state.name.x state.name.y green
  else render_box state.name.x state.name.y white;
  Glut.swapBuffers ()
