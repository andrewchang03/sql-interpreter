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
      | UP -> raise (Failure "Unimplemented")
      (* state.level.y <- state.level.y -. 200. *)
      | DOWN -> raise (Failure "Unimplemented")
    end
  | Enter -> raise (Failure "Unimplemented")

let init () =
  {
    day = init_days;
    level = init_level;
    name = init_name;
    select_pos = 1;
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
