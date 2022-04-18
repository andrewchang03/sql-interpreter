let font = Glut.BITMAP_HELVETICA_18

let drawString ?(font = font) x y s =
  GlMat.load_identity ();
  GlPix.raster_pos ~x ~y ();
  String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c)) s

let render () =
  GlClear.clear [ `color ];
  drawString ~font:Glut.BITMAP_TIMES_ROMAN_24 150. 290. "hello";
  GlMat.load_identity ();
  GlMat.translate3 (50., 550., 0.);
  GlDraw.color (255., 255., 255.);
  GlDraw.begins `line_loop;
  List.iter GlDraw.vertex2
    [ (-20., -5.); (-20., 5.); (20., 5.); (20., -5.) ];

  GlDraw.ends ();
  Glut.swapBuffers ()
