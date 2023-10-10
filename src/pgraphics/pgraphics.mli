(** This module contains various functions for rendering a GUI. Uses Pygame as a
    backend. *)

val init : unit -> unit
(** [initialize ()] initializes the graphics module. This must be called before
    any other functions in this library are called. *)

val init_display : int -> int -> unit
(** [init_display w h] creates a new display window with width [w] and height
    [h] *)

val set_caption : string -> unit
(** [set_caption s] changes the name of the window to [s] *)

type color = int * int * int
(** A color represented in [(R, G, B)] where [R], [G], and [B] are ints between
    0 and 255. *)

val fill : color -> unit
(** [fill (r, g, b)] fills the screen in with the color [(r, g, b)] *)

val flip : unit -> unit
(** [flip ()] refreshes the display window *)

val rect : int -> int -> int -> int -> color -> unit
(** [rect x y width height (r, g, b)] draws a rectangle on the screen. [(x, y)]
    is top left of rectangle *)

val line : int -> int -> int -> int -> int -> color -> unit
(** [line x1 y1 x2 y2 w c] draws a line from [(x1, y1)] to [(x2, y2)] with
    thickness [w] in color [c]. *)

val poly : (int * int) array -> color -> unit
(** [poly points color] draws a polygon with the given points and color. Points
    are from top left *)

val ellipse : int -> int -> int -> int -> color -> unit
(** [ellipse x y width height (r, g, b)] draws a ellipse on the screen. [(x, y)]
    is the center of the ellipse *)

val draw_text : string -> int -> int -> unit
(** [draw_text str x y] draws the text to the screen, with the top left point at
    [(x, y)] *)

val draw_text_center : string -> int -> int -> unit
(** [draw_text_center str x y] draws the text to the screen, with the center at
    [(x, y)] *)

val draw_text_left_align : string -> int -> int -> unit
(** [draw_text_left_align x y] draws text, with the left edge at [x] and
    vertically centered at [y]. *)

val draw_text_right_align : string -> int -> int -> unit
(** Same as [draw_text_left_align] but flipped horizontally *)

val get_mouse_click : unit -> (int * int) list
(** [get_mouse_click] is a list of the coordinates of every mouse click since
    the previous function call. *)

val check_quit : unit -> bool
(** [check_quit ()] is true if pygame has had a quit event *)

val draw_image : string -> int * int -> unit
(** [render_image s (x, y)] renders the image in the assets folder with the name
    [s] at position [(x, y)]. Images are saved after loading; drawing the same
    image multiple times will only result in one read from the file system.*)
