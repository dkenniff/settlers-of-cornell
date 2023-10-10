(** PyObject for pygame *)
let pygame = ref None

(** PyObject for [pygame.set_mode(width, height)] *)
let screen_ref = ref None

(** PyObject for pygame.gfxdraw. Necessary for anti-aliased drawn shapes *)
let gfxdraw_ref = ref None

(** PyObject for pygame.freetype. Necessary for drawing text to screen *)
let font_ref = ref None

(** [call x s a] calls the function with the name s in the python object x, with
    the arguments in a. *)
let call x s a = Py.Callable.to_function (Py.Object.find_attr_string (x ()) s) a

(** [get_ref s x] is the PyObject stored in [x] and throws an exception with
    error message [s] is [x] is None. *)
let get_ref s x () =
  match !x with
  | None -> failwith s
  | Some x -> x

(** Represents pygame *)
let py = get_ref "Graphics library not initialized" pygame

(** Represents pygame.gfxdraw *)
let gfxdraw = get_ref "Graphics library not initialized (gfxdraw)" gfxdraw_ref

(** Represents the screen object for the main display *)
let screen = get_ref "Screen not initialized" screen_ref

(** Represents pygame.freetype *)
let font = get_ref "Font not initialized" font_ref

(** [import s] imports the module with the name [s] to the python environment,
    and fails with a helpful error message if the import fails. *)
let import s =
  match Py.import_opt s with
  | None -> failwith (s ^ " import failed. Is " ^ s ^ " installed?")
  | Some x -> Some x

(** Alias of [Py.String.of_string] *)
let str = Py.String.of_string

let init () =
  Py.initialize ();
  pygame := import "pygame";
  gfxdraw_ref := import "pygame.gfxdraw";
  font_ref := import "pygame.freetype";
  call font "init" [||] |> ignore

(** [get s ()] is the property of name [s] from the pygame module. *)
let get s () = Py.Module.get (py ()) s

(** PyObject of [pygame.display] *)
let display = get "display"

(** PyObject of [pygame.draw] *)
let draw = get "draw"

(** PyObject of [pygame.event] *)
let event = get "event"

(** PyObject of [pygame.image] *)
let image = get "image"

(** [set_mode () size] calls [pygame.display.set_mode(size)]. *)
let set_mode () = call display "set_mode"

let set_caption s =
  call display "set_caption" [| Py.String.of_string s |] |> ignore

(** [int_array_to_py a] is an array with every number in [a] converted to a
    python integer. *)
let int_array_to_py (a : int array) =
  Py.Array.of_array Py.Int.of_int Py.Int.to_int a

let init_display width height =
  screen_ref := Some (set_mode () [| int_array_to_py [| width; height |] |])

type color = int * int * int

(** [color (r, g, b)] is a pygame color object of [(r, g, b)]. *)
let color (r, g, b) =
  let r, g, b = (Py.Int.of_int r, Py.Int.of_int g, Py.Int.of_int b) in
  Py.Tuple.of_array [| r; g; b |]

let fill x = call screen "fill" [| color x |] |> ignore
let flip () = call display "flip" [||] |> ignore

let rect x y w h c =
  let rect = call py "Rect" (Array.map Py.Int.of_int [| x; y; w; h |]) in
  call draw "rect"
    [| screen (); color c; rect; Py.Int.of_int 0; Py.Int.of_int 10 |]
  |> ignore

let line x y x' y' t c =
  let p1 = int_array_to_py [| x; y |] in
  let p2 = int_array_to_py [| x'; y' |] in
  let thickness = Py.Int.of_int t in
  call draw "line" [| screen (); color c; p1; p2; thickness |] |> ignore

let poly points c =
  let points_py =
    Array.map
      (fun (x, y) -> Py.Tuple.of_array [| Py.Int.of_int x; Py.Int.of_int y |])
      points
    |> Py.Array.of_array Fun.id Fun.id
  in
  call gfxdraw "aapolygon" [| screen (); points_py; color c |] |> ignore;
  call gfxdraw "filled_polygon" [| screen (); points_py; color c |] |> ignore

let ellipse x y rX rY c =
  call gfxdraw "aaellipse"
    [|
      screen ();
      Py.Int.of_int x;
      Py.Int.of_int y;
      Py.Int.of_int rX;
      Py.Int.of_int rY;
      color c;
    |]
  |> ignore;
  call gfxdraw "filled_ellipse"
    [|
      screen ();
      Py.Int.of_int x;
      Py.Int.of_int y;
      Py.Int.of_int rX;
      Py.Int.of_int rY;
      color c;
    |]
  |> ignore

let draw_text s x y =
  let offset = if int_of_string s >= 10 then 6 else 4 in
  let s = str s in
  let font_object = call font "SysFont" [| str "arial"; Py.Int.of_int 15 |] in
  call
    (fun () -> font_object)
    "render_to"
    [|
      screen ();
      Py.Tuple.of_pair (Py.Int.of_int (x - offset), Py.Int.of_int (y - 4));
      s;
    |]
  |> ignore

(** A font object to render fonts. All systems should have arial installed. *)
let font_object = lazy (call font "SysFont" [| str "arial"; Py.Int.of_int 15 |])

(** [blit i (x,  y)] is the same as [screen.blit(i, (x, y))]. *)
let blit i (x, y) =
  call screen "blit"
    [| i; Py.Tuple.of_pair (Py.Int.of_int x, Py.Int.of_int y) |]
  |> ignore

(** [draw_text_f s x y xf yf] draws [s] to the screen, at position
    [(xf x w, yf y h)] where [w] and [h] are the width and the height. This
    allows for easy text transformations. *)
let draw_text_f s x y xf yf =
  let s = str s in
  let surf, rect =
    call (fun () -> Lazy.force font_object) "render" [| s |] |> Py.Tuple.to_pair
  in
  let width = Py.Object.find_attr_string rect "width" |> Py.Int.to_int in
  let height = Py.Object.find_attr_string rect "height" |> Py.Int.to_int in
  blit surf (xf x width, yf y height)

(** [center x w] = [x - (w / 2)]. Helpful for centering objects *)
let center x w = x - (w / 2)

let draw_text_center s x y = draw_text_f s x y center center
let draw_text_left_align s x y = draw_text_f s x y (fun x _ -> x) center
let draw_text_right_align s x y = draw_text_f s x y (fun x w -> x - w) center

(** [get_event_type s] is a python list of every event matching string [s]. See
    pygame documentation for example values of [s]. *)
let get_event_type s = call event "get" [| get s () |]

let get_mouse_click () =
  get_event_type "MOUSEBUTTONUP"
  |> Py.List.to_list_map (fun e ->
         let x, y = Py.Object.find_attr_string e "pos" |> Py.Tuple.to_tuple2 in
         (Py.Int.to_int x, Py.Int.to_int y))

let check_quit () = get_event_type "QUIT" |> Py.List.length > 0

(** Stores image objects after they have been loaded from the file system. *)
let images : (string, Py.Object.t) Hashtbl.t = Hashtbl.create 4

let draw_image name (x, y) =
  match Hashtbl.find_opt images name with
  | None ->
      let img = call image "load" [| Py.String.of_string name |] in
      Hashtbl.add images name img;
      blit img (x, y)
  | Some img -> blit img (x, y)
