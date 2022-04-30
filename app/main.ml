open Database
open Interface

let appController game keyToAction ~key ~x ~y =
  match keyToAction ~key ~x ~y with
  | Some action -> game := Interface.controller !game action
  | None -> ()

let arrowKeys ~key ~x ~y =
  match key with
  | Glut.KEY_LEFT -> Some (Interface.move LEFT)
  | Glut.KEY_RIGHT -> Some (Interface.move RIGHT)
  | Glut.KEY_UP -> Some (Interface.move DOWN)
  | Glut.KEY_DOWN -> Some (Interface.move UP)
  | _ -> None

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
let initEngine ~state ~w ~h =
  initDisplay ~w ~h ~title:"Cornell CS Course Roster";
  initView ~w ~h;
  Glut.keyboardFunc ~cb:(appController state arrowKeys);
  Glut.displayFunc (fun () -> Interface.render ());
  Glut.mainLoop

(* This is the first function executed by OCaml *)
(* We init the game object reference and pass it to the engine *)
let () =
  ignore @@ Glut.init Sys.argv;
  let run = initEngine ~w:750 ~h:600 in
  run ()

(* LablGL graphics window initialization code credit to:
   https://github.com/marmelab/ocaml-invader *)
