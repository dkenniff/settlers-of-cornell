open Player

let points_to_win = 10
let board_size = 3
let num_dice = 2
let dice_size = 6
let max_players = 4

type robber = {
  mutable curr_tile : Hex.point;
  mutable needs_move : bool;
}
(** The type to store the necessary information for the robber game mechanic.
    The tile with the robber gives no resources on rolls. The robber must be
    moved whenever a 7 is rolled. *)

(** [init_robber b] initializes a [robber] for the game [b] by placing it on the
    first desert tile in the game, or the last tile in the game board if there
    is no desert tile. *)
let init_robber (b : Hex.t) =
  let rec get_desert_or_last = function
    | [] -> failwith "list too short"
    | [ h ] -> h
    | h :: t ->
        if Hex.get_color h b = Hex.Desert then h else get_desert_or_last t
  in
  let coord_lst = Hex.coordinates b in
  let p = get_desert_or_last coord_lst in
  { curr_tile = p; needs_move = false }

(** CircularList represents a circularly linked list that will be used to store
    the players of the game. *)
module CircularList = struct
  type 'a node = {
    value : 'a;
    mutable prev : 'a node option;
    mutable next : 'a node option;
  }
  (** AF: [{ value = x; prev = Some n1; next = Some n2 }] represents the
      circular list node with value [x] that is between nodes [n1] and [n2].

      RI: [prev] and [next] are not None. *)

  type 'a clist = {
    mutable first : 'a node option;
    mutable size : int;
  }
  (** AF: [{ first = Some \[node\]; size = x }] represents the circular list of
      size x where the active node is [node].

      RI: none *)

  exception EmptyCircularList
  (** [EmptyCircularList] is raised when user attempts to access information in
      an empty list. *)

  (** [get_val n] is the value of [n]. *)
  let get_val (n : 'a node) : 'a = n.value

  (** [get_next n] is the next node after n. *)
  let get_next (n : 'a node) : 'a node =
    match n.next with
    | None -> raise EmptyCircularList
    | Some k -> k

  (** [get_prev n] is the node before n. *)
  let get_prev (n : 'a node) : 'a node =
    match n.prev with
    | None -> raise EmptyCircularList
    | Some k -> k

  (** [empty] represents the empty circular list. *)
  let empty () : 'a clist = { first = None; size = 0 }

  (** [size cl] is the size of circular list [cl]. *)
  let size (cl : 'a clist) : int = cl.size

  (** [is_empty cl] is true iff [cl] represents the empty circular list. *)
  let is_empty (cl : 'a clist) : bool = size cl = 0

  (** [current_node cl] is the first node in [cl]. Requires: [cl] is not empty. *)
  let current_node (cl : 'a clist) : 'a node =
    match cl.first with
    | None -> raise EmptyCircularList
    | Some k -> k

  (** [next_node cl] is the next node in [cl]. Requires: [cl] is a non-empty
      list. *)
  let next_node (cl : 'a clist) : 'a node =
    if is_empty cl then raise EmptyCircularList
    else
      let first_node = current_node cl in
      match first_node.next with
      | Some k -> k
      | _ -> failwith "impossible"

  (** [add_node cl v] adds a new node with value [v] to clist [cl]. *)
  let add_node (cl : 'a clist) (v : 'a) : unit =
    let new_node = { value = v; prev = None; next = None } in
    if is_empty cl then (
      new_node.next <- Some new_node;
      new_node.prev <- Some new_node;
      cl.first <- Some new_node;
      cl.size <- 1)
    else
      let curr_node = current_node cl in
      let prev_node = get_prev curr_node in
      new_node.prev <- Some prev_node;
      new_node.next <- Some curr_node;
      prev_node.next <- Some new_node;
      curr_node.prev <- Some new_node;
      cl.size <- cl.size + 1

  (** [next cl] updates the first node in cl to the current node's next node. *)
  let next (cl : 'a clist) : unit =
    if is_empty cl then raise EmptyCircularList
    else cl.first <- Some (next_node cl)

  (** [remove_node v cl] removes the node with value [v] from [cl]. *)
  let remove_node (n : 'a node) (cl : 'a clist) : unit =
    if size cl = 1 || size cl = 0 then (
      cl.first <- None;
      cl.size <- 0)
    else (
      if current_node cl == n then next cl;
      let prev = get_prev n in
      let next = get_next n in
      prev.next <- Some next;
      next.prev <- Some prev;
      cl.size <- cl.size - 1)
end

open CircularList

type color = int * int * int
(** The type representing a color in RGB form. *)

(** possible colors for players. *)
let player_colors =
  [ (219, 0, 0); (223, 138, 0); (40, 18, 208); (255, 255, 255) ]

type t = {
  points_to_win : int;
  min_roll : int;
  max_roll : int;
  mutable num_players : int;
  mutable gameboard : Hex.t;
  players : Player.t clist;
  mutable next_player_id : int;
  robber : robber;
  mutable acceptable_colors : color list;
}

let create_new_game () =
  let board = Hex.make_board board_size in
  {
    points_to_win;
    min_roll = num_dice;
    max_roll = num_dice * dice_size;
    num_players = 0;
    gameboard = board;
    players = CircularList.empty ();
    next_player_id = 0;
    robber = init_robber board;
    acceptable_colors = player_colors;
  }

exception PlayerLimit
exception PlayerNotFound

let get_map (g : t) : Hex.t = g.gameboard
let set_map g h = g.gameboard <- h
let num_players (g : t) : int = g.num_players

(** [get_rand_el_of_list lst] is a random element of [lst]. *)
let get_rand_el_of_list lst =
  let n = Random.int (List.length lst) in
  let rec get_nth n = function
    | [] -> failwith "list index greater than length"
    | h :: t -> if n = 0 then h else get_nth (n - 1) t
  in
  get_nth n lst

(** [pick_rand_clr g] is a random color selected from the list of acceptable
    colors found in [g.acceptable_colors] and updates [g.acceptable_colors] to
    no longer include the selected color. *)
let pick_rand_clr g =
  let rec remove_from_lst c lst =
    match lst with
    | [] -> []
    | h :: t -> if h = c then t else h :: remove_from_lst c t
  in
  let clr = get_rand_el_of_list g.acceptable_colors in
  g.acceptable_colors <- remove_from_lst clr g.acceptable_colors;
  clr

(** [start_player game player] takes and initialized [Player.t], [player], and a
    valid [game] and randomly places a valid settlement and road to be assigned
    to [player] as well as assign the resources of the tiles surrounding the
    newly placed settlement. *)
let start_player (game : t) (player : Player.t) : unit =
  let rec pick_actions () =
    let coord = get_rand_el_of_list (Hex.coordinates game.gameboard) in
    let vertex = Random.int 5 in
    if Hex.get_building coord vertex game.gameboard = Hex.EmptyBuilding then
      if Hex.check_empty_adjacent coord vertex game.gameboard then (
        game.gameboard <-
          Hex.set_building (Hex.Town player) coord vertex game.gameboard;
        game.gameboard <-
          Hex.set_road (Hex.Road player) coord vertex game.gameboard;
        Player.add_structure player Player.TownStructure 1;
        Player.add_structure player Player.RoadStructure 1;
        Player.add_point player)
      else pick_actions ()
    else pick_actions ()
  in
  pick_actions ()

let add_new_player (game : t) (name : string) : unit =
  if num_players game = max_players then raise PlayerLimit
  else
    let new_player = init_player game.next_player_id name in
    set_color new_player (pick_rand_clr game);
    add_node game.players new_player;
    game.next_player_id <- game.next_player_id + 1;
    game.num_players <- num_players game + 1

let current_player (g : t) : Player.t = g.players |> current_node |> get_val
let next_turn g = next g.players

let get_all_players (g : t) : Player.t array =
  try
    let player_array = Array.make (num_players g) (current_player g) in
    let curr_player = ref (g.players |> current_node) in
    for i = 0 to Array.length player_array - 1 do
      player_array.(i) <- get_val !curr_player;
      curr_player := get_next !curr_player
    done;
    player_array
  with EmptyCircularList -> Array.make 0 (init_player 0 "")

(** [get_player_node start_node n s] is the player node with id value [n] in the
    list of nodes that [start_node] is included in.

    Raises: [PlayerNotFound] if the player does not exist in the game, which
    happens when [s = 0]. *)
let rec get_player_node (cl : Player.t node) (n : int) (s : int) : Player.t node
    =
  if s = 0 then raise PlayerNotFound
  else if player_id (get_val cl) = n then cl
  else get_player_node (get_next cl) n (s - 1)

let get_player (g : t) (n : int) : Player.t =
  try get_val (get_player_node (current_node g.players) n (num_players g))
  with EmptyCircularList -> raise PlayerNotFound

let remove_player (g : t) (p : Player.t) : unit =
  try
    let player_color = get_color p in
    g.acceptable_colors <- player_color :: g.acceptable_colors;
    remove_node
      (get_player_node (current_node g.players) (player_id p) (num_players g))
      g.players;
    g.num_players <- size g.players
  with EmptyCircularList -> ()

type roll = int * int

let values r = r

(** [single_die ()] is the number rolled for a single die in the game *)
let single_die () = Random.int (dice_size - 1) + 1

(** [execute_specific_val v g] executes a roll with the value [v] in game [g]. *)
let execute_specific_val (v : int) (g : t) : unit =
  if v = 7 then g.robber.needs_move <- true
  else
    let map = g.gameboard in
    Hex.coordinates map
    |> List.iter (fun h ->
           if Hex.get_number h map = Some v && h <> g.robber.curr_tile then
             match Hex.get_color h map with
             | Resource r ->
                 for v = 0 to 5 do
                   match Hex.get_building h v map with
                   | Town i -> add_resource i r 1
                   | City i -> add_resource i r 2
                   | EmptyBuilding -> ()
                 done
             | Desert | None -> ())

let execute_specific_roll a b g =
  let sum = a + b in
  execute_specific_val sum g;
  (a, b)

let execute_roll g =
  let a, b = (single_die (), single_die ()) in
  execute_specific_roll a b g

let give_player_resources (g : t) : unit =
  let rec ex_rolls v =
    if v < g.min_roll then ()
    else (
      if v = 7 then ex_rolls (v - 1) else execute_specific_val v g;
      ex_rolls (v - 1))
  in
  ex_rolls g.max_roll

type build_action = {
  player : Player.t;
  hex : int * int;
  ve : int;
  structure : [ `Road of Hex.road | `Building of Hex.building ];
}

let new_vertex_action p b loc v =
  { player = p; hex = loc; ve = v; structure = `Building b }

let new_edge_action p b loc v =
  { player = p; hex = loc; ve = v; structure = `Road b }

(** [check_connected g b] is [true] iff there is at least one road in the
    gameboard of [g] neighboring the desired road placement edge that belongs to
    the player in build action [b].

    Note: This function will return [true] if the desired road placement edge
    already has a road belonging to the player there, despite that [b] is
    invalid. See [check_road_action]. *)
let check_connected g b =
  let connected_roads =
    Hex.get_adjacent_roads b.hex b.ve g.gameboard
    @ Hex.get_adjacent_roads b.hex ((b.ve + 1) mod 6) g.gameboard
  in
  List.fold_left
    (fun acc r -> acc || r = Hex.Road b.player)
    false connected_roads

(** [check_road_action g b] is [true] iff [b] is a valid road-building action in
    the game [g].

    Requires: The structure field of [b] is of type [`Road]. *)
let check_road_action g b =
  match b.structure with
  | `Building building -> failwith "precondition violated"
  | `Road road ->
      road = Hex.Road b.player
      && Hex.get_road b.hex b.ve g.gameboard = Hex.EmptyRoad
      && check_connected g b
      && num_resource b.player Brick >= 1
      && num_resource b.player Lumber >= 1
      && Player.num_structure b.player RoadStructure < 15

(** [check_other_roads b surrounding_roads] is a helper for [check_roads], and
    specifically makes sure that the roads that are connected to vertex where
    the player is trying to place a building, and do not belong to the player
    allow for the *)
let check_other_roads b surrounding_roads =
  let other_roads =
    List.filter (fun r -> r <> Hex.Road b.player) surrounding_roads
  in
  match other_roads with
  | [ e1; e2 ] -> (e1 = EmptyRoad && e2 = EmptyRoad) || e1 <> e2
  | _ -> failwith "impossible"

(** [check_roads g b] returns true iff the states of the roads in [g] would
    allow for [b] to be a valid building action.

    Requires: The structure field of [b] is of type [`Building]. *)
let check_roads g b =
  let surrounding_roads = Hex.get_adjacent_roads b.hex b.ve g.gameboard in
  let player_road_count =
    List.fold_left
      (fun acc r -> if r = Hex.Road b.player then acc + 1 else acc)
      0 surrounding_roads
  in
  match player_road_count with
  | 0 -> false
  | 1 -> check_other_roads b surrounding_roads
  | 2 | 3 -> true
  | _ -> failwith "impossible"

(** [check_town_action g b building] returns true if [b] is a valid town or
    city-building action in the game [g], and false if not.

    Requires: The structure field of [b] is of type [`Building]. *)
let check_town_action g b building =
  building = Hex.Town b.player
  && Hex.get_building b.hex b.ve g.gameboard = EmptyBuilding
  && num_resource b.player Brick >= 1
  && num_resource b.player Lumber >= 1
  && num_resource b.player Livestock >= 1
  && num_resource b.player Metal >= 1
  && Player.num_structure b.player TownStructure < 4

(** [check_city_action g b building] returns true if [b] is a valid town or
    city-building action in the game [g], and false if not.

    Requires: The structure field of [b] is of type [`Building]. *)
let check_city_action g b building =
  building = Hex.City b.player
  && Hex.get_building b.hex b.ve g.gameboard = Hex.Town b.player
  && num_resource b.player Ore >= 3
  && num_resource b.player Metal >= 2
  && Player.num_structure b.player CityStructure < 5

(** [check_building_action g b] returns true if [b] is a valid town or
    city-building action in the game [g], and false if not.

    Requires: The structure field of [b] is of type [`Building]. *)
let check_building_action g b =
  match b.structure with
  | `Road building -> failwith "precondition violated"
  | `Building building -> (
      Hex.check_empty_adjacent b.hex b.ve g.gameboard
      && check_roads g b
      &&
      match building with
      | Hex.Town _ -> check_town_action g b building
      | Hex.City _ -> check_city_action g b building
      | _ -> false)

let validate_action g b =
  (*
  ***Conditions Needing to be Satisfied:*** 

  coordinates must be valid
  vertex/edge must be valid

   ***For Roads Only:*** 
  new road needs to belong to player
  if attempting to place road, current road at target edge has to be EmptyRoad
  road must be connecting to existing road belonging to player
  player has to have required resources: 1 Brick and 1 Lumber
  can only place at most 15 roads

   ***For Buildings Only:*** 
  new building needs to belong to player
  buildings have to be at least 2 roads away from any other (anyone's) buildings
  building must be connected to existing road of same player
  if >=2/3 edges connecting to a vertex belong to one player, only that player
    can place a building at that vertex

   ***For Buildings that are Specifically Cities Only:*** 
  current building at target vertex has be a Town belonging to the player of
    the build action
  player has to have required resources: 3 Ore and 2 Metal
  can only place at most 5 cities

   ***For Buildings that are Specifically Towns Only:*** 
  player has to have required resources: 1 Brick, 1 Lumber, 1 Livestock, 1
    Metal
  current building at target vertex has to be EmptyBuilding
  can only place at most 4 towns

   ************************************************************************    
   *)
  g.gameboard |> Hex.coordinates |> List.mem b.hex
  && b.ve >= 0 && b.ve <= 5
  &&
  match b.structure with
  | `Road road -> check_road_action g b
  | `Building building -> check_building_action g b

let take_action g b =
  match b.structure with
  | `Road road ->
      add_resource b.player Brick (-1);
      add_resource b.player Lumber (-1);
      add_structure b.player RoadStructure 1;
      g.gameboard |> Hex.set_road (Hex.Road b.player) b.hex b.ve |> set_map g
  | `Building building -> begin
      match building with
      | Hex.Town town ->
          let cost = [ Brick; Lumber; Livestock; Metal ] in
          List.iter (fun r -> add_resource b.player r (-1)) cost;
          add_structure b.player TownStructure 1;
          add_point b.player;
          g.gameboard
          |> Hex.set_building (Hex.Town b.player) b.hex b.ve
          |> set_map g
      | Hex.City city ->
          add_resource b.player Ore (-3);
          add_resource b.player Metal (-2);
          add_structure b.player TownStructure (-1);
          add_structure b.player CityStructure 1;
          add_point b.player;
          g.gameboard
          |> Hex.set_building (Hex.City b.player) b.hex b.ve
          |> set_map g
      | Hex.EmptyBuilding -> failwith "precondition violated (invalid action)"
    end

let check_winner g =
  g |> get_all_players
  |> Array.find_opt (fun p -> num_points p >= points_to_win)

let needs_move_robber (game : t) : bool = game.robber.needs_move

let move_robber (game : t) (pt : Hex.point) : unit =
  try
    ignore (List.find (fun e -> e = pt) (Hex.coordinates game.gameboard));
    game.robber.curr_tile <- pt;
    game.robber.needs_move <- false
  with Not_found -> failwith "TileOutOfBounds"

let curr_loc_robber (game : t) : Hex.point = game.robber.curr_tile
