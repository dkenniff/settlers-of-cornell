(** Representation of dynamic player data.

    This module represents a player's information as they are playing the game,
    including their player id, the buildings they own, the roads they own, the
    resources they own, and current number of points they have. *)

type t
(** The abstract type of values representing players. *)

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

val init_player : int -> string -> t
(** [init_player id name] is the initial player with the player id [id] and the
    name [name] when the game begins. In that state the player has no buildings
    and no resources. *)

val player_id : t -> int
(** [player_id p] is the player id number of the player [p]. *)

val player_name : t -> string
(** [player_name p] is the player name of the player [p]. *)

val num_resource : t -> resource -> int
(** [num_resource p r] is the number of resource [r] that the player currently
    has. *)

val add_resource : t -> resource -> int -> unit
(** [add_resource p r amt] mutates player [p] to have [amt] more of resource
    [r]. Use a negative value of [amt] to remove resources. Raises
    [Invalid_argument] when attempting to remove more resources than the player
    has *)

val num_structure : t -> structure -> int
(** [num_structure p s] is the number of structure [s] that the player currently
    has. *)

val add_structure : t -> structure -> int -> unit
(** [add_resource p s amt] mutates player [p] to have [amt] more of structure
    [s]. Use a negative value of [amt] to remove structures. Requires: [amt] is
    either 1 or -1, as a player is always creating only one structure at a time
    or replacing one with another. *)

val num_points : t -> int
(** [get_points p] is the number of victory points [p] has *)

val add_point : t -> unit
(** [add_point p] adds one point to [p]'s total number of points. *)

val get_color : t -> int * int * int
(** [get_color p] is the color for the player *)

val set_color : t -> int * int * int -> unit
(** [set_color p c] sets the color of the player to be [c] *)
