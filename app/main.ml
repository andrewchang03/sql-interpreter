open Database
open Interface

let appController state keyToAction ~key ~x ~y =
  match keyToAction ~key with
  | Some action -> Interface.controller !state action
  | None -> ()

let arrowKeys ~key =
  match key with
  | Glut.KEY_LEFT -> Some (Interface.Move LEFT)
  | Glut.KEY_RIGHT -> Some (Interface.Move RIGHT)
  | Glut.KEY_UP -> Some (Interface.Move DOWN)
  | Glut.KEY_DOWN -> Some (Interface.Move UP)
  | _ -> None

let enterKey ~key =
  match key with
  | 13 -> Some Interface.Enter
  | _ -> None

(* Declare rendering function, buffering mode, and create window *)
let initDisplay ~w ~h ~title =
  Glut.initDisplayMode ~double_buffer:true ~depth:true ~alpha:true ();
  Glut.initWindowSize ~w ~h;
  ignore @@ Glut.createWindow ~title;
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
  Glut.specialFunc ~cb:(appController state arrowKeys);
  Glut.keyboardFunc ~cb:(appController state enterKey);
  Glut.displayFunc (fun () -> Interface.render !state);
  Glut.mainLoop

(* This is the first function executed by OCaml *)
(* We init the game object reference and pass it to the engine *)
let () =
  ignore @@ Glut.init Sys.argv;
  let state = ref (Interface.init ()) in
  let run = initEngine ~state ~w:750 ~h:800 in
  run ()

(* LablGL graphics window initialization code credit to:
   https://github.com/marmelab/ocaml-invader *)
