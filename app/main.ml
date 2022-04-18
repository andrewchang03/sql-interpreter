open Database
open Interface

(* Declare rendering function, buffering mode, and create window *)
let initDisplay ~w ~h ~title =
  Glut.initDisplayMode ~double_buffer:true ~depth:true ~alpha:true ();
  Glut.initWindowSize ~w ~h;
  Glut.createWindow ~title;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay)

(* Initialize OpenGL rendering options *)
let initView ~w ~h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.ortho2d ~x:(0.0, float_of_int w) ~y:(0.0, float_of_int h);
  GlMat.mode `modelview

(* Initialize each part of the game engine *)
(* Then, it returns a callable mainLoop *)
let initEngine ~w ~h =
  initDisplay ~w ~h ~title:"Cornell CS Course Roster";
  initView ~w ~h;
  Glut.displayFunc (fun () -> Interface.render ());
  Glut.mainLoop

(* This is the first function executed by OCaml *)
(* We init the game object reference and pass it to the engine *)
let () =
  ignore @@ Glut.init Sys.argv;
  let run = initEngine ~w:600 ~h:600 in
  run ()

(* LablGL graphics window initialization code credit to:
   https://github.com/marmelab/ocaml-invader *)
