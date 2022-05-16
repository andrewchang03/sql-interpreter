(** Provides functionalities to render the GUI based on key presses. *)

type direction =
  | LEFT
  | RIGHT
  | UP
  | DOWN
(* [direction] describes the four directions corresponding to arrow key
   inputs. *)

type action =
  | Move of direction
  | Enter
(* [action] describes the two possible key actions *)

type state
(* [state] stores information on the app's current state. *)

val controller : state -> action -> unit
(* [controller s a] directly modifies state [s] based on action [a]. *)

val init : unit -> state
(* [init ()] gives the initial state of the app. *)

val render : state -> unit
(* [render s] is called after each key action to graphically display the
   state [s]. *)
