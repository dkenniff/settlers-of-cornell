(** Representation of miscellaneous game information.

    This module provides basic definitions for constants and functions that will
    be used and referenced by other modules and are the basic building blocks of
    the game rules. *)

val points_to_win : int
(** The constant representing the number of victory points needed for a player
    to win. *)

val board_size : int
(** The constant representing the default size of the gameboard being used .*)

val num_dice : int
(** The constant representing the number of dice being played with (minimum: 1). *)

val dice_size : int
(** The number of sides of the dice being played with (minimum of 1). *)

val max_players : int
(** The maximum number of players allowed in a game. *)

type t
(** Type representing a game. *)

exception PlayerLimit
(** [PlayerLimit] is raised when the number of players registered is equal to
    the maximum number of players allowed and the user attempts to add a new
    player. *)

exception PlayerNotFound
(** [PlayerNotFound] is raised when the user attempts to access a player that
    does not exist. *)

val create_new_game : unit -> t
(** [create_new_game] creates a new game with a randomized gameboard, no
    players, and the game constants set. *)

val get_map : t -> Hex.t
(** [get_map t] is the current hex map of game [t]. *)

val set_map : t -> Hex.t -> unit
(** [set_map t h] sets the map of game [t] to be [h]. *)

val num_players : t -> int
(** [num_players t] returns the number of currently registered players in game
    [t]. *)

val add_new_player : t -> string -> unit
(** [add_new_player game name] adds a new player with name [name] to the game
    [game]. Raises: [PlayerLimit] if trying to add more than the max number of
    players. *)

val start_player : t -> Player.t -> unit
(** [start_player game player] takes and initialized [Player.t], [player], and a
    valid [game] and randomly places a valid settlement and road to be assigned
    to [player] as well as assign the resources of the tiles surrounding the
    newly placed settlement. *)

val give_player_resources : t -> unit
(** [give_player_resources g] assigns each player the proper resources for each
    tile surrounding a town or city that they own. *)

val remove_player : t -> Player.t -> unit
(** [remove_player game player] removes Player.t [player] from game [game]. *)

val get_player : t -> int -> Player.t
(** [get_player game id] is the Player.t with the associated id value of [id]. *)

val get_all_players : t -> Player.t array
(** [get_all_players game] is the array of players that are playing [game]. *)

val current_player : t -> Player.t
(** [current_player g] is the player whose turn it currently is in [g] *)

val next_turn : t -> unit
(** [next_turn g] advances [g] to the next player's turn *)

type roll
(** A single roll event *)

val values : roll -> int * int
(** [values r] is a list containing the dice values in [r]. Example: if [r]
    represented rolling a 6 and a 4, [roll_value r] could return [(6, 4)] *)

val execute_roll : t -> roll
(** [execute_roll g] is a newly created roll event that was executed on game
    [g]. Side effects: mutates [g] with the new roll. *)

val execute_specific_roll : int -> int -> t -> roll
(** [execute_specific_roll a b g] is a newly created roll event that was
    executed on game [g] with dice values [a] and [b]. Side effects: mutates [g]
    with the new roll. *)

type build_action
(** A single building action that a player could take. This is a proposed
    action. It may be a valid action or an invalid action. It may represent
    building on an edge or building on a vertex. It is NOT directly linked to a
    game or current game state (except indirectly though the player value). For
    example: a value of this type could represent "player 1" building a city on
    the 4th vertex of hex (0, 0). *)

val new_vertex_action :
  Player.t -> Hex.building -> Hex.point -> int -> build_action
(** [new_build_action p b loc v] is a value that represents player [p], building
    a building of type [b], on point [loc], at vertex [v]. *)

val new_edge_action : Player.t -> Hex.road -> Hex.point -> int -> build_action
(** [new_edge_action p b loc v] is a value that represents player [p], building
    a building of type [b], on edge [loc], at vertex [v]. *)

val validate_action : t -> build_action -> bool
(** [validate_action g b] is true iff [b] is a valid build action on the current
    state of [g]. *)

val take_action : t -> build_action -> unit
(** [take_action g b] mutates [g] to take action [b]. Requires: [b] is a valid
    action. *)

val check_winner : t -> Player.t option
(** [check_winner g] is Some [p] where [p] is the winning player or None for no
    winning player *)

val needs_move_robber : t -> bool
(** [needs_move_robber g] returns true if the robber needs to be moved in game
    [g], i.e. a 7 has been rolled and the robber has not yet been moved since,
    or false otherwise.*)

val move_robber : t -> Hex.point -> unit
(** [move_robber g p] moves the robber in game [g] from its current location to
    coordinate [p]. *)

val curr_loc_robber : t -> Hex.point
(** [curr_loc_robber g] returns the current coordinate point of the robber in
    game [g]. Raises: TileOutOfBounds if [p] is not a valid coordinate of the
    gameboard of [g]. *)
