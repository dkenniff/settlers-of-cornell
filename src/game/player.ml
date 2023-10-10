type resource =
  | Brick
  | Metal
  | Ore
  | Livestock
  | Lumber

type structure =
  | RoadStructure
  | TownStructure
  | CityStructure

type color = int * int * int
(** The type representing a color in RGB form *)

type player_resources = (resource * int) array
(** The type representing the amount of each resource a player has

    RI: Each resource type must have EXACTLY 1 element in the list that
    represents it. *)

type player_structures = (structure * int) array
(** The type representing the amount of each structure a player has

    RI: Each structure type must have EXACTLY 1 element in the list that
    represents it. *)

type t = {
  player_id : int;
  player_name : string;
  building_list : int list;
  road_list : (int * int) list;
  mutable resources : player_resources;
  mutable structures : player_structures;
  mutable points : int;
  mutable color : int * int * int;
}

(** [init_resources] creates an initialized record of type [player_resources]
    with all field values set to 0. *)
let init_resources () =
  [| (Brick, 2); (Metal, 2); (Ore, 2); (Livestock, 2); (Lumber, 2) |]

(** [init_structures] creates an initialized record of type [player_structures]
    with all field values set to 0. *)
let init_structures () =
  [| (RoadStructure, 0); (TownStructure, 0); (CityStructure, 0) |]

let init_player (id : int) (name : string) : t =
  {
    player_id = id;
    player_name = name;
    building_list = [];
    road_list = [];
    resources = init_resources ();
    structures = init_structures ();
    points = 0;
    color = (0, 0, 0);
  }

let player_id (player : t) : int = player.player_id
let player_name (player : t) : string = player.player_name

let num_resource p r =
  Array.fold_left
    (fun acc (res, amt) -> if res = r then acc + amt else acc)
    0 p.resources

let add_resource p r amt =
  Array.iteri
    (fun i (k, v) ->
      if k = r then
        if v + amt < 0 then
          raise
            (Invalid_argument
               "Cannot remove more of a resource than the player has")
        else p.resources.(i) <- (k, v + amt))
    p.resources

let num_structure p s =
  Array.fold_left
    (fun acc (structure, amt) -> if structure = s then acc + amt else acc)
    0 p.structures

let add_structure p s amt =
  Array.iteri
    (fun i (k, v) -> if k = s then p.structures.(i) <- (k, v + amt))
    p.structures

let num_points p = p.points
let add_point p = p.points <- p.points + 1
let get_color p = p.color
let set_color p c = p.color <- c
