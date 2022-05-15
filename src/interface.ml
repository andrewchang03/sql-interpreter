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
  mutable t : Csv.t;
}

let init_days : day_selector = { x = 100.; y = 400. }
let init_level : level_selector = { x = 300.; y = 350. }
let init_name : name_selector = { x = 500.; y = 400. }
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

let controller state action =
  match action with
  | Move direction -> begin
      match direction with
      | LEFT ->
          if state.select_pos == 1 then ()
          else if state.select_pos == 2 then state.select_pos <- 1
          else if state.select_pos == 3 then state.select_pos <- 2
      | RIGHT ->
          if state.select_pos == 3 then ()
          else if state.select_pos == 2 then state.select_pos <- 3
          else if state.select_pos == 1 then state.select_pos <- 2
      | UP ->
          if state.select_pos == 1 && state.day.y > 300. then
            state.day.y <- state.day.y -. 50.
          else if state.select_pos == 2 && state.level.y > 300. then
            state.level.y <- state.level.y -. 50.
          else if state.select_pos == 3 && state.name.y > 300. then
            state.name.y <- state.name.y -. 50.
      | DOWN ->
          if state.select_pos == 1 && state.day.y < 400. then
            state.day.y <- state.day.y +. 50.
          else if state.select_pos == 2 && state.level.y < 400. then
            state.level.y <- state.level.y +. 50.
          else if state.select_pos == 3 && state.name.y < 400. then
            state.name.y <- state.name.y +. 50.
    end
  | Enter ->
      let day_query =
        let table =
          Csv.load ("data" ^ Filename.dir_sep ^ "csvClasses" ^ ".csv")
        in
        if state.day.y == 300. then
          Table.select_where_table table "time" EQ "Other"
        else if state.day.y == 350. then
          Table.select_where_table table "time" EQ "Tues/Thurs"
        else Table.select_where_table table "time" EQ "Mon/Wed/Fri"
      in
      let level_query =
        if state.level.y == 300. then
          Table.select_where_table
            (Table.select_where_table day_query "id" GE "5000")
            "id" LESS "7000"
        else if state.level.y == 350. then
          Table.select_where_table
            (Table.select_where_table day_query "id" GE "3000")
            "id" LESS "5000"
        else
          Table.select_where_table
            (Table.select_where_table day_query "id" GE "1000")
            "id" LESS "3000"
      in
      let name_query =
        if state.name.y == 300. then
          Table.select_where_table level_query "name" GREATER "Q"
        else if state.level.y == 350. then
          Table.select_where_table
            (Table.select_where_table level_query "name" GREATER "G")
            "name" LESS "P"
        else Table.select_where_table level_query "name" LESS "G"
      in
      state.t <- name_query

let init () =
  {
    day = init_days;
    level = init_level;
    name = init_name;
    select_pos = 1;
    t = Csv.load ("data" ^ Filename.dir_sep ^ "csvClasses" ^ ".csv");
  }

let render (state : state) =
  GlClear.clear [ `color ];
  drawString 100. 530.
    "1. Use arrow keys to select your search options.";
  drawString 100. 500. "2. Press the enter key to submit the query.";
  drawString ~font:Glut.BITMAP_TIMES_ROMAN_24 100. 450. "Days Offered";
  drawString ~font:Glut.BITMAP_TIMES_ROMAN_24 300. 450. "Course Level";
  drawString ~font:Glut.BITMAP_TIMES_ROMAN_24 500. 450. "Course Name";
  drawString 100. 400. "Mon/Wed/Fri";
  drawString 100. 350. "Tues/Thurs";
  drawString 100. 300. "Other";
  drawString 300. 400. "1000-2999";
  drawString 300. 350. "3000-4999";
  drawString 300. 300. "5000-7999";
  drawString 500. 400. "A-F";
  drawString 500. 350. "G-P";
  drawString 500. 300. "Q-Z";
  if state.select_pos == 1 then
    render_box state.day.x state.day.y (0., 255., 0.)
  else render_box state.day.x state.day.y (255., 255., 255.);
  if state.select_pos == 2 then
    render_box state.level.x state.level.y (0., 255., 0.)
  else render_box state.level.x state.level.y (255., 255., 255.);
  if state.select_pos == 3 then
    render_box state.name.x state.name.y (0., 255., 0.)
  else render_box state.name.x state.name.y (255., 255., 255.);
  Glut.swapBuffers ()
