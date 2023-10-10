type color =
  | Resource of Player.resource
  | Desert
  | None

type building =
  | Town of Player.t
  | City of Player.t
  | EmptyBuilding

type road =
  | Road of Player.t
  | EmptyRoad

type point = int * int

type tile = {
  coord : point;
  mutable vertices : building array;
  mutable edges : road array;
  color : color;
  number : int option;
}
(** Type of a single hex tile *)

type t = tile list

let size t = List.length t
let coordinates t = List.map (fun tile -> tile.coord) t

(** [x -- y] is a list enumerated from x to y (inclusive).

    Source: OCaml Programming: Correct + Efficient + Beautiful by Michael R.
    Clarkson et al. (2022)*)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

(** [cartesian_self lst] generates a list containing all pairs contained in the
    cartesian product of [lst] with itself. Source:
    https://stackoverflow.com/questions/10893521/how-to-take-product-of-two-list-in-ocaml*)
let rec cartesian_self lst =
  List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1, e2)) lst) lst)

(** [remove_too_large n lst] removes coordinates that are too large to be apart
    of a board of size [n]. *)
let rec remove_too_large n lst =
  List.filter (fun e -> abs (fst e + snd e) <= n) lst

let from_size s =
  -s -- s |> cartesian_self |> remove_too_large s
  |> List.map (fun coord ->
         {
           coord;
           vertices = Array.make 6 EmptyBuilding;
           edges = Array.make 6 EmptyRoad;
           color = None;
           number = None;
         })

exception VertexOutOfBounds
exception EdgeOutOfBounds
exception TileOutOfBounds

(** [get_tile c t] is the tile in [t] with coordinates [c]. *)
let get_tile c t =
  try List.find (fun e -> e.coord = c) t
  with Not_found -> raise TileOutOfBounds

let set_color color coord t =
  List.map
    (fun tile -> if tile.coord = coord then { tile with color } else tile)
    t

let set_number number coord t =
  List.map
    (fun tile -> if tile.coord = coord then { tile with number } else tile)
    t

let get_color p t = (get_tile p t).color

let get_number p t =
  let tile = get_tile p t in
  tile.number

let get_building p v t =
  if v < 0 || v > 5 then raise VertexOutOfBounds
  else (get_tile p t).vertices.(v)

let get_road p e t =
  if e < 0 || e > 5 then raise EdgeOutOfBounds else (get_tile p t).edges.(e)

(** [update_building p v building t] is the tile in [t] located at [p] with its
    vertex at [v] updated to be [building]. Requires: [p] is a valid coordinate
    in [t]. *)
let update_building p v building t =
  let t1 = get_tile p t in
  t1.vertices.(v) <- building;
  t1

(** [update_buildings p1 v1 p2 v2 p3 v3 building t] is a new [t] with the
    buildings at [v1] of [p1], [v2] of [p2], and [v3] of [p3] changed to be
    [building].

    Requires: [p1] is the coordinates for the tile to be updated, and [p2] and
    [p3] are the coordinates which share [v1] (vertex of [p1]) at their own
    vertices [v2] and [v3], respectively. The tiles at any of these coordinates
    may or may not be in [t] (The returned [t] will only be updated at valid
    coordinates), but the vertices must be integers in the range 0 to 5. *)
let update_buildings p1 v1 p2 v2 p3 v3 building t =
  List.map
    (fun tile ->
      if tile.coord = p1 then update_building p1 v1 building t
      else if tile.coord = p2 then update_building p2 v2 building t
      else if tile.coord = p3 then update_building p3 v3 building t
      else tile)
    t

(** [overlapping_vertex x y v] is the two other coordinate forms of vertex [v]
    of hex [(x, y)]. *)
let overlapping_vertex k j = function
  | 0 -> ((k, j - 1), 2, (k + 1, j - 1), 4)
  | 1 -> ((k + 1, j - 1), 3, (k + 1, j), 5)
  | 2 -> ((k + 1, j), 4, (k, j + 1), 0)
  | 3 -> ((k, j + 1), 5, (k - 1, j + 1), 1)
  | 4 -> ((k - 1, j + 1), 0, (k - 1, j), 2)
  | 5 -> ((k - 1, j), 1, (k, j - 1), 3)
  | _ -> raise VertexOutOfBounds

let set_building building (k, j) v t =
  let c1, v1, c2, v2 = overlapping_vertex k j v in

  update_buildings (k, j) v c1 v1 c2 v2 building t

(** [update_road p e road t] is the tile in [t] located at [p] with its edge at
    [e] updated to be [road].

    Requires: [p] is a valid coordinate in [t]. *)
let update_road p e road t =
  let t1 = get_tile p t in
  t1.edges.(e) <- road;
  t1

(** [update_roads p1 e1 p2 e2 building t] returns a new [t] with the roads at
    [e1] of [p1] and [e2] of [p2] changed to be [road].

    Requires: [p1] is the coordinates for the tile to be updated, and [p2] is
    the tile which shares [e1] (edge of [p1]) at their own edge [e2],
    respectively. The tiles at any of these coordinates may or may not be in [t]
    (The returned [t] will only be updated at valid coordinates), but the edges
    must be integers in the range 0 to 5. *)
let update_roads p1 e1 p2 e2 road t =
  List.map
    (fun tile ->
      if tile.coord = p1 then update_road p1 e1 road t
      else if tile.coord = p2 then update_road p2 e2 road t
      else tile)
    t

let set_road road (j, k) e t =
  let edge, new_edge =
    match e with
    | 0 -> ((j + 1, k - 1), 3)
    | 1 -> ((j + 1, k), 4)
    | 2 -> ((j, k + 1), 5)
    | 3 -> ((j - 1, k + 1), 0)
    | 4 -> ((j - 1, k), 1)
    | 5 -> ((j, k - 1), 2)
    | _ -> raise EdgeOutOfBounds
  in
  update_roads (j, k) e edge new_edge road t

(** [( %+ )] is x mod y, but always returns a positive value. *)
let ( %+ ) x y = if x >= 0 then x mod y else y - (-1 * x mod y)

let check_empty_adjacent (x, y) v t =
  get_building (x, y) ((v - 1) %+ 6) t = EmptyBuilding
  && get_building (x, y) ((v + 1) %+ 6) t = EmptyBuilding
  &&
  try
    match v with
    | 0 -> get_building (x, y - 1) 1 t = EmptyBuilding
    | 1 -> get_building (x + 1, y - 1) 2 t = EmptyBuilding
    | 2 -> get_building (x + 1, y) 3 t = EmptyBuilding
    | 3 -> get_building (x, y + 1) 4 t = EmptyBuilding
    | 4 -> get_building (x - 1, y + 1) 5 t = EmptyBuilding
    | 5 -> get_building (x - 1, y) 0 t = EmptyBuilding
    | _ -> raise VertexOutOfBounds
  with TileOutOfBounds -> true

let get_adjacent_roads (x, y) v t =
  let roads = Array.make 3 EmptyRoad in
  roads.(0) <- get_road (x, y) v t;
  roads.(1) <- get_road (x, y) ((v - 1) %+ 6) t;
  (try
     match v with
     | 0 -> roads.(2) <- get_road (x, y - 1) 1 t
     | 1 -> roads.(2) <- get_road (x + 1, y - 1) 2 t
     | 2 -> roads.(2) <- get_road (x + 1, y) 3 t
     | 3 -> roads.(2) <- get_road (x, y + 1) 4 t
     | 4 -> roads.(2) <- get_road (x - 1, y + 1) 5 t
     | 5 -> roads.(2) <- get_road (x - 1, y) 0 t
     | _ -> raise VertexOutOfBounds
   with TileOutOfBounds -> roads.(2) <- EmptyRoad);
  Array.to_list roads

(** [pick_random_color] is a pseudorandom resource from the available resources.
    Each resource has a 4/21 chance of being chosen, and desert has a 1/21
    chance of being chosen. *)
let pick_random_color unit : color =
  let rand_int = Random.int 21 in
  if rand_int = 0 then Desert
  else if rand_int <= 4 then Resource Player.Brick
  else if rand_int <= 8 then Resource Player.Lumber
  else if rand_int <= 12 then Resource Player.Ore
  else if rand_int <= 16 then Resource Player.Livestock
  else Resource Player.Metal

(** [random_tile_number ()] is a pseudorandom int in the range 2..12 and not
    equal to 7. *)
let rec random_tile_number unit : int =
  let i = Random.int 10 + 2 in
  if i = 7 then random_tile_number () else i

let make_board (s : int) : t =
  from_size s
  |> List.map (fun t ->
         {
           t with
           color = pick_random_color ();
           number = Some (random_tile_number ());
         })

let canonical_vertex (x, y) v (t : t) =
  let candidates =
    begin
      begin
        match v with
        | 0 -> [ (2, x, y - 1); (4, x + 1, y - 1) ]
        | 1 -> [ (3, x + 1, y - 1); (5, x + 1, y) ]
        | 2 -> [ (4, x + 1, y); (0, x, y + 1) ]
        | 3 -> [ (1, x - 1, y + 1); (5, x, y + 1) ]
        | 4 -> [ (2, x - 1, y); (0, x - 1, y + 1) ]
        | 5 -> [ (3, x, y - 1); (1, x - 1, y) ]
        | _ -> raise VertexOutOfBounds
      end
      |> List.filter (fun (v, x, y) ->
             List.exists (fun c -> c.coord = (x, y)) t)
    end
  in
  let vertex, cx, cy =
    List.fold_left
      (fun (v, x, y) (v', x', y') -> if v < v' then (v, x, y) else (v', x', y'))
      (v, x, y) candidates
  in
  ((cx, cy), vertex)

let canonical_edge p e t =
  let x, y = p in
  let new_p, new_e =
    match e with
    | 0 -> ((x, y), 0)
    | 1 -> ((x, y), 1)
    | 2 -> ((x, y), 2)
    | 3 -> ((x - 1, y + 1), 0)
    | 4 -> ((x - 1, y), 1)
    | 5 -> ((x, y - 1), 2)
    | _ -> raise VertexOutOfBounds
  in
  match List.find_opt (fun point -> point = new_p) (coordinates t) with
  | Some p -> (new_p, new_e)
  | None -> (p, e)
