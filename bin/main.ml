(** main.ml - Main game loop and rendering code

    This file contains the main game loop, as well as the code for rendering a
    game state.*)

open Async
open Consts;;

Random.self_init ()

(** Estimation value of the square root of 3 *)
let sqrt3 = 0.8660254

type button = {
  pos : int * int;
  size : int * int;
  action : unit -> unit;
  mutable visible : bool;
  color : Pgraphics.color;
  path : string option;
  mutable active : bool;
}
(** Button displayed on the screen.

    [pos] is upper left corner

    [path] is file name of an image to display as the button

    [color] is unused if [path] is not [None]

    [visible] determines if the button is rendered

    [active] determines if the button's action will be called when it is clicked *)

(** Basic button with default values pre-filled *)
let default_button =
  {
    pos = (0, 0);
    size = (0, 0);
    action = (fun () -> ());
    visible = true;
    color = Colors.button_default;
    path = None;
    active = true;
  }

(** Represents the current state of the game.

    [TitleScreen] - Before the game starts

    [AwaitingRoll] - User must click roll button before continuing.

    [AwaitingBuildingSelection] - User has indicated that they wish to build
    something, and need to select the location to build in.

    [AwaitingPlayerAction] - User has rolled the dice, awaiting their next move.

    [AwaitingRobberMove] - User has rolled a 7 and must move the robber before
    continuing.

    [PlayerWon s] - Player with name [s] has won the game. *)
type game_state =
  | TitleScreen
  | AwaitingRoll
  | AwaitingBuildingSelection
  | AwaitingPlayerAction
  | AwaitingRobberMove
  | PlayerWon of string

type screen_data = {
  mutable game : Game.Gameobjects.t;
  mutable buttons : button array;
  mutable state : game_state;
  mutable dice : string * string;
}
(** All information relating to the current screen *)

(** [print s] is [print_endline s] if debug printing is enabled. Otherwise do
    nothing. *)
let print s = if Consts.ProfileOptions.debug_print then print_endline s

(** Main screen for the application *)
let rec screen =
  {
    game =
      (let g = Game.Gameobjects.create_new_game () in
       Game.Gameobjects.add_new_player g "Player 1";
       Game.Gameobjects.add_new_player g "Player 2";
       Game.Gameobjects.add_new_player g "Player 3";
       Game.Gameobjects.add_new_player g "Player 4";
       g);
    dice = ("assets/dice/0.png", "assets/dice/0.png");
    buttons =
      [|
        (* Roll dice button *)
        {
          default_button with
          pos = RollButton.pos;
          size = RollButton.size;
          path = Some "assets/roll.png";
          action =
            (fun () ->
              if screen.state = AwaitingRoll then begin
                (let r =
                   Game.Gameobjects.execute_roll screen.game
                   |> Game.Gameobjects.values
                 in
                 print (Format.sprintf "ROLL (%d, %d)" (fst r) (snd r));
                 screen.dice <-
                   ( Format.sprintf "assets/dice/%d.png" (fst r),
                     Format.sprintf "assets/dice/%d.png" (snd r) ));
                screen.state <-
                  (if Game.Gameobjects.needs_move_robber screen.game then
                   AwaitingRobberMove
                  else AwaitingPlayerAction)
              end);
        };
        (* Next turn button *)
        {
          default_button with
          pos = NextTurn.pos;
          size = NextTurn.size;
          path = Some "assets/next.png";
          action =
            (fun () ->
              if screen.state = AwaitingPlayerAction then
                match Game.Gameobjects.check_winner screen.game with
                | Some p ->
                    screen.state <- PlayerWon (Game.Player.player_name p)
                | None ->
                    Game.Gameobjects.next_turn screen.game;
                    screen.state <- AwaitingRoll;
                    print "NEXT TURN CALLED");
        };
        (* Enter build mode button *)
        {
          default_button with
          pos = BuildTown.pos;
          size = BuildTown.size;
          path = Some "assets/build.png";
          action =
            (fun () ->
              if screen.state = AwaitingPlayerAction then (
                screen.state <- AwaitingBuildingSelection;
                print "Awaiting building selection"));
        };
        start_button;
      |];
    state = TitleScreen;
  }

(** Button takes up the whole screen on start, then deactivates when user clicks
    to start the game *)
and start_button =
  {
    default_button with
    pos = StartButton.pos;
    size = StartButton.size;
    visible = false;
    action =
      (fun () ->
        start_button.active <- false;
        if screen.state = TitleScreen then screen.state <- AwaitingRoll);
  }

(** [convert_color c] is the Graphics module representation of [c]. *)
let convert_color = function
  | Game.Hex.Resource x -> (
      let open Game.Player in
      let open Colors in
      match x with
      | Brick -> brick
      | Livestock -> livestock
      | Lumber -> lumber
      | Ore -> ore
      | Metal -> metal)
  | Game.Hex.Desert -> Colors.desert
  | Game.Hex.None -> Colors.empty_tile

(** [hex_points (x, y) r c n] is an array of points that correspond to a hexagon
    centered at [(x, y)] with side length [r]. *)
let hex_points (x, y) r =
  [|
    (x, y -. r);
    (x +. (sqrt3 *. r), y -. (r /. 2.));
    (x +. (sqrt3 *. r), y +. (r /. 2.));
    (x, y +. r);
    (x -. (sqrt3 *. r), y +. (r /. 2.));
    (x -. (sqrt3 *. r), y -. (r /. 2.));
  |]

(** [draw_hex (x, y) r c n] draws a hexagon to the screen, centered at point (x,
    y) with side length [r], and of color [c], with text [n] drawn in the
    center. If [n] is an empty string, the circle in the middle of the hex is
    not drawn. *)
let draw_hex (x, y) r c n =
  let points_int =
    let points = hex_points (x, y) r in

    Array.map (fun (x, y) -> (int_of_float x, int_of_float y)) points
  in
  Pgraphics.poly points_int c;
  if n <> "" then (
    let rad = r *. number_size_ratio |> int_of_float in
    Pgraphics.ellipse (int_of_float x) (int_of_float y) rad rad
      Colors.interior_circle;
    Pgraphics.draw_text_center n (int_of_float x) (int_of_float y))

(** [convert_coordinates (x, y) (cx, cy) r] converts coordinates in a hex grid
    (x, y) (see implementation of hex.ml) to the center of that hex on the
    screen (cx, cy) based on size length [r]. *)
let convert_coordinates (x, y) (cx, cy) r =
  let x = cx + (2 * r * x) + (y * r) |> float_of_int in
  let y =
    float_of_int cy +. (float_of_int y *. 2. *. sqrt3 *. float_of_int r)
  in
  (x, y)

(** [draw_map_helper b l] draws every hex in [l] to the screen. Drawn slightly
    bigger if [b] is true. Will not draw numbers on desert tiles. *)
let draw_map_helper border h =
  let x, y =
    convert_coordinates h
      (screen_width / 2, int_of_float (float_of_int screen_height /. 2.7))
      radius
  in
  let number =
    if
      Game.Hex.get_color h (Game.Gameobjects.get_map screen.game)
      = Game.Hex.Desert
      || Game.Gameobjects.curr_loc_robber screen.game = h
    then ""
    else
      match Game.Hex.get_number h (Game.Gameobjects.get_map screen.game) with
      | Some n -> string_of_int n
      | None -> ""
  in
  draw_hex (x, y)
    (float_of_int radius *. if border then 1.08 else 1.28)
    (if border then
     Game.Hex.get_color h (Game.Gameobjects.get_map screen.game)
     |> convert_color
    else Colors.hex_border_color)
    (if border then number else "")

let draw_robber_hex () =
  let h = Game.Gameobjects.curr_loc_robber screen.game in
  let h' =
    convert_coordinates h
      (screen_width / 2, int_of_float (float_of_int screen_height /. 2.7))
      radius
  in
  draw_hex h' (float_of_int radius *. 0.75) Consts.Colors.hex_border_color ""

(** [draw_map m] draws [m] to the screen. *)
let draw_map m =
  let hexes = Game.Hex.coordinates m in
  List.iter (draw_map_helper false) hexes;
  List.iter (draw_map_helper true) hexes;
  draw_robber_hex ()

(** [draw_button b] draws b to the screen. *)
let draw_button b =
  if b.visible then
    match b.path with
    | None ->
        let (x, y), (w, h) = (b.pos, b.size) in
        Pgraphics.rect x y w h b.color
    | Some p ->
        let x, y = b.pos in
        Pgraphics.draw_image p (x, y)

(** Create a module that draws the amount of a certain type of resource a player
    has to the screen. *)
module ResourceDrawer (Resource : ResourceLabelType) = struct
  let draw game player =
    let open Resource in
    Pgraphics.rect x y width height c;
    Pgraphics.ellipse (x + 23) (y + 23) (width - 30) (height - 30)
      Colors.interior_circle;

    let x, y = Resource.center in
    Pgraphics.draw_text_center
      (Game.Player.num_resource player r |> string_of_int)
      x y
end

module Lumber = ResourceDrawer (ResourceLabels.Lumber)
module Livestock = ResourceDrawer (ResourceLabels.Livestock)
module Ore = ResourceDrawer (ResourceLabels.Ore)
module Brick = ResourceDrawer (ResourceLabels.Brick)
module Metal = ResourceDrawer (ResourceLabels.Metal)

(** Draw number of each resource that each player has to the screen *)
let draw_resources () =
  let current_player = Game.Gameobjects.current_player screen.game in
  Lumber.draw Colors.lumber current_player;
  Livestock.draw Colors.livestock current_player;
  Ore.draw Colors.ore current_player;
  Brick.draw Colors.brick current_player;
  Metal.draw Colors.metal current_player

(** [cur_name ()] is the name of the current player *)
let cur_name () =
  Game.Gameobjects.current_player screen.game |> Game.Player.player_name

(** [help_text ()] is the current text to display to the player *)
let help_text () =
  match screen.state with
  | TitleScreen -> ""
  | AwaitingRoll -> cur_name () ^ ": Make a roll"
  | AwaitingBuildingSelection -> cur_name () ^ ": Select a location to build"
  | AwaitingPlayerAction ->
      cur_name () ^ ": Enter build mode or select 'Next turn'"
  | AwaitingRobberMove ->
      cur_name () ^ ": You rolled a 7! Select a new hex for the robber"
  | PlayerWon s -> s ^ " has won the game!"

(** [draw_dice ()] draws the last-rolled dice on the screen *)
let draw_dice () =
  let d1, d2 = screen.dice in
  Pgraphics.draw_image d1 (Dice1.x, Dice1.y);
  Pgraphics.draw_image d2 (Dice2.x, Dice2.y)

(** [draw_hand ()] draws the information about the current player's hand to the
    screen. (Buttons are not drawn, they are rendered in [draw_button]) *)
let draw_hand () =
  let open Hand in
  Pgraphics.rect x y width height Colors.hand_background;
  draw_resources ();
  draw_dice ();
  let x, y = center in
  Pgraphics.rect
    (Hand.x + (Hand.width / 2) - 250)
    (Hand.y - 25) 500 25 Consts.Colors.desert;
  Pgraphics.draw_text_center (help_text ()) x (y - (height / 2) - 10)

(** [average_points lst] is the average of the first elements, and the average
    of the second elements. *)
let average_points lst =
  let tx, ty =
    List.fold_left (fun (x, y) (x', y') -> (x +. x', y +. y')) (0., 0.) lst
  in
  let l = List.length lst |> float_of_int in
  (tx /. l, ty /. l)

(** [for_every_vertex map f] executes [f] for every vertex in [map].

    [f (hx, hy) i (x, y)] is the function where [(hx, hy)] are the hex
    coordinates in the map; [i] is the vertex number; and [(x, y)] are the
    coordinates of the vertex on the screen. *)
let for_every_vertex map (f : int * int -> int -> float * float -> unit) =
  let all_points = Hashtbl.create 120 in

  List.map
    (fun (hx, hy) ->
      let x, y =
        convert_coordinates (hx, hy)
          (screen_width / 2, int_of_float (float_of_int screen_height /. 2.7))
          radius
      in
      Array.mapi
        (fun i (x, y) ->
          let h, v =
            Game.(
              Hex.canonical_vertex (hx, hy) i (Gameobjects.get_map screen.game))
          in
          Hashtbl.replace all_points (h, v)
            (match Hashtbl.find_opt all_points (h, v) with
            | None -> [ (x, y) ]
            | Some lst -> (x, y) :: lst))
        (hex_points (x, y) (float_of_int radius)))
    (Game.Hex.coordinates map)
  |> ignore;
  Hashtbl.iter
    (fun (h, v) coords_list -> f h v (average_points coords_list))
    all_points

(** [for_every game target_function] executes the target function for every edge
    in the map of [game]

    [target_function h i p1 p2] where [h] is the hex-coordinates of the hex; [i]
    is the edge number; [p1] and [p2] are the screen coordinates of the two
    points of the edge *)
let for_every game
    (target_function :
      Game.Hex.t -> int * int -> int -> float * float -> float * float -> unit)
    =
  let map = Game.Gameobjects.get_map game in
  List.map
    (fun (hx, hy) ->
      let x, y =
        convert_coordinates (hx, hy)
          (screen_width / 2, int_of_float (float_of_int screen_height /. 2.7))
          radius
      in
      (* There is space between hexes, increase radius to push edge buttons out
         from the center slightly *)
      let vertex_points = hex_points (x, y) (float_of_int radius *. 1.16) in
      Array.mapi
        (fun i edge_num ->
          (* Edges are between two vertex. Extract the coords of the vertex they
             are between *)
          let (x1, y1), (x2, y2) =
            (vertex_points.(i), vertex_points.((i + 1) mod 6))
          in
          target_function map (hx, hy) i (x1, y1) (x2, y2))
        vertex_points)
    (Game.Hex.coordinates map)
  |> ignore

let road_points (x1, y1) (x2, y2) =
  let r = 0.2 in
  let x = (~-.r *. x1) +. (r *. x2) +. x1 in
  let y = (~-.r *. y1) +. (r *. y2) +. y1 in
  let r' = 1. -. r in
  let x' = (~-.r' *. x1) +. (r' *. x2) +. x1 in
  let y' = (~-.r' *. y1) +. (r' *. y2) +. y1 in

  ((x, y), (x', y'))

(** [draw_every_edge game] draws roads (if present) for every edge in [game] *)
let draw_every_edge game =
  for_every game (fun map (hx, hy) i (x1, y1) (x2, y2) ->
      match Game.Hex.get_road (hx, hy) i map with
      | EmptyRoad -> ()
      | Road p ->
          let (x1, y1), (x2, y2) = road_points (x1, y1) (x2, y2) in
          Game.Player.get_color p
          |> Pgraphics.line (int_of_float x1) (int_of_float y1)
               (int_of_float x2) (int_of_float y2) 5)

(** [draw_every_edge game f] applies [f] to every edge in [game] *)
let for_every_edge game f =
  for_every game (fun map (hx, hy) i (x1, y1) (x2, y2) ->
      f (hx, hy) i ((x1 +. x2) /. 2., (y1 +. y2) /. 2.))

(** [add_building_buttons map action_maker display_string size for_every_function]
    adds a button for every possible location the user could click on to request
    a building. These could be edges or vertices.

    [map] - hexes to draw from

    [action_maker player h i] must be the build action that is created when
    [player] clicks on location [h] at edge/vertex num [i]

    [display_string] is a string that will be formatted with the edge/vertex
    number and the x and y coordinates of the hex and then displayed to console
    out

    [size] the edge size for the buttons

    [for_every_function] one of [for_every_edge] or [for_every_vertex]. See
    their specs *)
let add_building_buttons map
    (action_maker :
      Game.Player.t -> Game.Hex.point -> int -> Game.Gameobjects.build_action)
    display_string (size : int) for_every_function =
  for_every_function map (fun (hx, hy) i ((cx, cy) : float * float) ->
      screen.buttons <-
        Array.append screen.buttons
          [|
            {
              default_button with
              pos = (int_of_float cx - (size / 2), int_of_float cy - (size / 2));
              size = (size, size);
              action =
                (fun () ->
                  if screen.state = AwaitingBuildingSelection then (
                    screen.state <- AwaitingPlayerAction;
                    print (Format.sprintf display_string i hx hy);

                    let action =
                      action_maker
                        (Game.Gameobjects.current_player screen.game)
                        (hx, hy) i
                    in
                    if Game.Gameobjects.validate_action screen.game action then
                      Game.Gameobjects.take_action screen.game action));
              visible = false;
            };
          |])

(** Add a button over top of every edge on the map for building roads *)
let add_edge_buttons map =
  add_building_buttons map
    (fun player h i ->
      Game.Gameobjects.new_edge_action player (Road player) h i)
    "Attempting building road at edge %d at (%d, %d) clicked" 15 for_every_edge

(** Add a button over top of every vertex on the map for building cities/towns *)
let add_vertex_buttons map =
  add_building_buttons map
    (fun player h i ->
      let new_structure =
        if
          Game.Gameobjects.get_map screen.game
          |> Game.Hex.get_building h i = EmptyBuilding
        then Game.Hex.Town player
        else City player
      in
      Game.Gameobjects.new_vertex_action player new_structure h i)
    "Attempting building town/city at vertex %d at (%d, %d) clicked" 20
    for_every_vertex

(** [add_single_hex_button h] adds a button to move the robber at hex
    coordinates [h] *)
let add_single_hex_button h =
  let x, y =
    convert_coordinates h
      (screen_width / 2, int_of_float (float_of_int screen_height /. 2.7))
      radius
  in
  screen.buttons <-
    Array.append screen.buttons
      [|
        {
          default_button with
          pos = (int_of_float x - 20, int_of_float y - 20);
          size = (40, 40);
          visible = false;
          action =
            (fun () ->
              if screen.state = AwaitingRobberMove then
                Game.Gameobjects.move_robber screen.game h;
              screen.state <- AwaitingPlayerAction);
        };
      |]

(** [add_hex_buttons ()] adds buttons to move the robber to every hex *)
let add_hex_buttons () =
  List.map
    (fun (x, y) -> add_single_hex_button (x, y))
    (Game.Gameobjects.get_map screen.game |> Game.Hex.coordinates)

(** Draw all towns *)
let draw_towns map =
  for_every_vertex map (fun (hx, hy) i (x, y) ->
      let x', y' = (int_of_float x, int_of_float y) in
      match Game.Hex.get_building (hx, hy) i map with
      | City t ->
          let s = 9 in
          Game.Player.get_color t
          |> Pgraphics.poly
               [| (x', y' + s); (x' + s, y'); (x', y' - s); (x' - s, y') |]
      | Town t -> Game.Player.get_color t |> Pgraphics.ellipse x' y' 5 5
      | _ -> ())

(** File path of the title screen *)
let title_image = "assets/title.png"

(** File path of the costs image*)
let costs_image = "assets/costs.png"

(** Render the title screen *)
let draw_title_screen () = Pgraphics.draw_image title_image (0, 0)

(** Render a player's icon card. Drawn in the upper right as a type of
    scoreboard *)
let render_card p (x, y) =
  Pgraphics.rect x y 100 50 (Game.Player.get_color p);
  Pgraphics.draw_text_left_align (Game.Player.player_name p) (x + 10) (y + 25);
  Pgraphics.draw_text_right_align
    (Game.Player.num_points p |> string_of_int)
    (x + 90) (y + 25)

(** Render every player's card *)
let render_player_cards () =
  Pgraphics.draw_text_center "VICTORY POINTS" (620 + (100 / 2)) 20;
  Game.Gameobjects.get_all_players screen.game
  |> Array.iteri (fun i p -> render_card p (620, (70 * i) + 40))

(** [render s] draws the scene [s] tox the screen *)
let render s =
  Pgraphics.fill Colors.bg;
  Pgraphics.draw_image "assets/background.png" (0, 0);
  begin
    match s.state with
    | TitleScreen -> draw_title_screen ()
    | _ ->
        draw_map (Game.Gameobjects.get_map screen.game);
        draw_hand ();
        draw_towns (Game.Gameobjects.get_map s.game);
        draw_every_edge s.game;
        render_player_cards ();
        Pgraphics.draw_image costs_image (30, 50);
        Array.iter draw_button s.buttons
        (* Uncomment this line to add a credits tag at the top of the main
           game *)
        (* Pgraphics.draw_text_left_align "CS 3110 FA22 | Mason Raffo, Dylan
           Kenniff, Adam Cahall" 10 10 *)
  end;
  Pgraphics.flip ()

(** [handle_click s (x, y)] checks if the coordinates (x, y) are inside any
    button of [s], and if so, calls the action function. If there are
    overlapping buttons, only one will be clicked (at random). *)
let handle_click s (x, y) =
  let c =
    Array.find_opt
      (fun b ->
        let (bx, by), (w, h) = (b.pos, b.size) in
        b.active && bx <= x && bx + w >= x && by <= y && by + h >= y)
      s.buttons
  in
  match c with
  | Some b -> b.action ()
  | None -> ()

(** Profile frame rate and display the results to the console *)
let profile_exec f str =
  if ProfileOptions.enabled then begin
    let start = Core.Time.now () in
    f ();
    let final = Core.Time.now () in
    if ProfileOptions.display then
      print
        (Format.sprintf "%s: %s" str
           (Core.Time.diff final start |> Core.Core_private.Span_float.to_string))
  end
  else f ()

(** Event loop *)
let rec loop () =
  profile_exec (fun () -> render screen) "Render time";

  profile_exec
    (fun () ->
      List.map (handle_click screen) (Pgraphics.get_mouse_click ()) |> ignore)
    "Click time";

  if Pgraphics.check_quit () then Shutdown.shutdown 0
  else Clock.after (Core.sec 0.1) >>> loop

let () =
  Pgraphics.init ();
  Pgraphics.init_display screen_width screen_height;
  Pgraphics.set_caption Consts.window_title;
  add_vertex_buttons (Game.Gameobjects.get_map screen.game);
  add_edge_buttons screen.game;
  add_hex_buttons () |> ignore;

  Array.map
    (fun p ->
      Game.Player.add_resource p Game.Player.Brick 10;
      Game.Player.add_resource p Game.Player.Lumber 10;
      Game.Player.add_resource p Game.Player.Ore 10;
      Game.Player.add_resource p Game.Player.Livestock 10;
      Game.Player.add_resource p Game.Player.Metal 10;
      Game.Gameobjects.start_player screen.game p;
      Game.Gameobjects.start_player screen.game p)
    (Game.Gameobjects.get_all_players screen.game)
  |> ignore;

  loop ();
  Core.never_returns (Scheduler.go ())
