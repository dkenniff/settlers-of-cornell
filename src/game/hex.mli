(** This module generates and maintains a map of hex tiles, as well as their
    edges and vertices. *)

type t
(** The abstract type of values representing a hex grid. *)

(** Type of the color of a tile. Can either be a resource, a desert, or
    uncolored. *)
type color =
  | Resource of Player.resource
  | Desert
  | None

(** Type of a building location. Exists on a vertex. *)
type building =
  | Town of Player.t
  | City of Player.t
  | EmptyBuilding

(** Type of a road location. Exists on an edge. *)
type road =
  | Road of Player.t
  | EmptyRoad

type point = int * int
(** Coordinate for a hex in the grid. Hexes are oriented with a vertex straight
    up and a vertex straight down. [(0,0)] is the center of the grid.

    [(x - 1, y)] is directly to the left of [(x, y)]

    [(x + 1, y)] is directly to the right of [(x, y)]

    [(x - 1, y + 1)] is to the bottom left of [(x, y)]

    [(x, y + 1)] is to the bottom right [(x, y)]

    [(x, y - 1)] is to the top left of [(x, y)]

    [(x + 1, y - 1)] is to the top right of [(x, y)] *)

val size : t -> int
(** [size t] is the number of hex tiles in the grid *)

val coordinates : t -> point list
(** [coordinates t] is all the valid coordinates of t *)

val from_size : int -> t
(** [from_size x] is a grid of empty hexes with dimension [x]. Requires:
    [x >= 0] *)

val set_color : color -> point -> t -> t
(** [set_color c p t] is the hex grid t with the tile at p changed to color c.
    Requires: [p] is a valid coordinate of [t]. *)

val set_number : int option -> point -> t -> t
(** [set_color c n t] is the hex grid t with the tile at p changed to number n.
    Requires: [p] is a valid coordinate of [t]. *)

val get_color : point -> t -> color
(** [get_color p t] is the color of the tile at point p. Requires: [p] is a
    valid coordinate of [t]. *)

val get_number : point -> t -> int option
(** [get_number p t] is the number of the tile at point p. Requires: [p] is a
    valid coordinate of [t].*)

val make_board : int -> t
(** [make_board n] is a game board of size n where all of the tile colors and
    numbers are randomly selected from the set of valid values. Requires:
    [n >= 0] *)

val get_building : point -> int -> t -> building
(** [get_building p v t] is building at vertex [v] of point [p] in [t].

    [v = 0] represents the top-most vertex, and vertices are indexed clockwise
    up to 5.

    Raises [VertexOutOfBounds] if v > 5 or v < 0. Raises [TileOutOfBounds] if
    [p] is not a valid coordinate of [t]. *)

val get_road : point -> int -> t -> road
(** [get_road p e t] is road at edge [e] of point [p] in [t].

    [e = 0] represents the top-right edge, and edges are index clockwise up to
    5.

    Raises [EdgeOutOfBounds] if e > 5 or e < 0. Raises [TileOutOfBounds] if [p]
    is not a valid coordinate of [t]. *)

val set_building : building -> point -> int -> t -> t
(** [set_building building p v t] is [t] with the building at vertex [v] of
    point [p] in [t] changed to be [building].

    [v = 0] represents the top-most vertex, and vertices are indexed clockwise
    up to 5.

    Note: the vertex is shared with two other hexes.

    Raises [VertexOutOfBounds] if v > 5 or v < 0. *)

val set_road : road -> point -> int -> t -> t
(** [set_road road p e t] is [t] with the road at edge [e] of point [p] in [t]
    changed to be [road]

    [e = 0] represents the top-right edge, and edges are index clockwise up to
    5.

    Note: the edge is shared with another hex.

    Raises [EdgeOutOfBounds] if e > 5 or e < 0.*)

val check_empty_adjacent : point -> int -> t -> bool
(** [check_empty_adjacent p v t] returns true if there are no buildings located
    at any of vertices adjacent to vertex [v] of the tile located at [p] in [t],
    and false otherwise. *)

val get_adjacent_roads : point -> int -> t -> road list
(** [get_adjacent_roads p v t] is a list of length 3 containing the three roads
    which connect to vertex [v] of the tile located at [p] in [t].

    Note: If [v] is on the edge of the board, it will actually only have 2
    adjacent roads that are part of the board. If this is the case, the entry of
    the list corresponding to the third adjacent road (which is out-of-bounds)
    will be [EmptyRoad]. *)

exception EdgeOutOfBounds
(** Raised when trying to access an edge that doesn't exist. *)

exception VertexOutOfBounds
(** Raised when trying to access a vertex that doesn't exist. *)

val canonical_vertex : point -> int -> t -> point * int
(** [canonical_vertex hex v t] is an equivalent vertex to the given vertex.
    Specifically, this function returns the same values for all equivalent
    vertex coordinates. *)

val canonical_edge : point -> int -> t -> point * int
(** [canonical_vertex hex e t] is an equivalent edge to the given edge.
    Specifically, this function returns the same values for all equivalent edge
    coordinates. *)
