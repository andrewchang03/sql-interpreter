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
  mutable table : string list list;
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

let rec rec_row x y row =
  match row with
  | [] -> ()
  | h :: t ->
      drawString (100. +. (x *. 80.)) y h;
      rec_row (x +. 1.) y t

let rec render_rows pos y rows =
  match rows with
  | [] -> ()
  | h :: t ->
      if pos <= 6. then (
        rec_row 0. y h;
        render_rows (pos +. 1.) (250. -. (50. *. pos)) t)
      else ()

let render_table x y color rows =
  match List.nth_opt rows 0 with
  | Some col ->
      let num_rows = List.length rows |> float_of_int in
      let num_col = List.length col |> float_of_int in
      render_rows 1. 250. rows;
      GlMat.load_identity ();
      GlMat.translate3 (x, y, 0.);
      GlDraw.color color;
      GlDraw.begins `line_loop;
      List.iter GlDraw.vertex2
        [
          (-10., 50. +. (40. *. num_rows));
          (-10., 0.);
          (50. +. (80. *. num_col), 0.);
          (50. +. (80. *. num_col), 50. +. (40. *. num_rows));
        ];
      GlDraw.color (255., 255., 255.);
      GlDraw.ends ()
  | None -> ()

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
  | Enter -> raise (Failure "Unimplemented")

let init () =
  {
    day = init_days;
    level = init_level;
    name = init_name;
    select_pos = 1;
    table = [ [ "row 1"; "row 2"; "row 3" ]; [ "row 2" ]; [ "row 3" ] ];
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
  drawString 300. 400. "2000-2999";
  drawString 300. 350. "3000-3999";
  drawString 300. 300. "4000-4999";
  drawString 500. 400. "A-F";
  drawString 500. 350. "G-P";
  drawString 500. 300. "Q-Z";
  let num_rows = float_of_int (List.length state.table) in
  render_table 100.
    (40. +. (10. *. num_rows))
    (255., 255., 255.) state.table;
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
