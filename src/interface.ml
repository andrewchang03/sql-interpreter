type direction =
  | LEFT
  | RIGHT
  | UP
  | DOWN

let select_pos = 0
let select_coors = (0., 0.)
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

let move (dir : direction) =
  match dir with
  | LEFT ->
      render_box (fst select_coors) (snd select_coors) (0., 0., 0.);
      if select_pos == 1 then
        render_box 500. (snd select_coors) (0., 255., 0.)
      else
        render_box
          (fst select_coors +. 200.)
          (snd select_coors) (0., 255., 0.)
  | RIGHT -> raise (Failure "Unimplemented")
  | UP -> raise (Failure "Unimplemented")
  | DOWN -> raise (Failure "Unimplemented")

let render () =
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
  render_box 300. 350. (255., 255., 255.);
  render_box 500. 400. (255., 255., 255.);
  render_box 100. 400. (0., 255., 0.);
  select_pos = 1;
  select_coors = (100., 400.);
  Glut.swapBuffers ()