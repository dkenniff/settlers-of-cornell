(* ======================== TEST PLAN ========================

   Our project had two major systems: the GUI frontend and the game logic
   backend. The GUI code includes /bin/ and Pgraphics, and it renders the game
   and handles all user interaction. It is difficult to automatically test a
   GUI, so these modules were thoroughly tested by hand. This testing includes
   both visual inspection (to ensure the game "looks right") and interaction, to
   ensure the frontend properly connects to the backend.

   The backend code includes the modules Hex, Player, and Gameobjects. Their
   dependency graph is as follows:

   Gameobjects relies on Player and Hex

   Hex relies on Player

   There is another module contained within Gameobjects, CircularList. This
   module is not test explicitly because it is not exposed in the Gameobjects
   mli file. It is tested implicitly as part of Gameobjects.

   We relied on a combination of black box and glass box testing for these
   backend modules. We did include some randomized testing for Hex.make_board.

   Our testing demonstrates our system is correct through extensive code
   coverage and completeness. Our tests aim to test every "path through the
   spec" of each backend function. Additionally, we used bisect to check our
   test coverage. Our test coverage was extremely good. 93.64% overall test
   coverage. 93% for Gameobjects, 92% for Hex, and 100% for Player. Almost all
   of the untested lines are failures for precondition violations.

   In addition to the 154 OUnit test cases defined, the GUI that displays the
   game helps demonstrate the correctness of the system significantly. It helped
   us to uncover a few additional, minor bugs that slipped under the radar of
   the OUnit test suite. *)

open OUnit2
open Game
open Hex
open Player
open Gameobjects

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. Source: A2 *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let size_test (name : string) (s : int) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (s |> from_size |> size) ~printer:string_of_int

let coordinates_test (name : string) (s : int) (expected_output : point list) :
    test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    (s |> from_size |> coordinates)

(** All the relative coordinates of overlaps between hexes and their vertexes.
    For example: the element [(v, (v1, x1, y1), (v2, x2, y2))] indicates that at
    hex (j, k), vertex v is the same as vertex v1 of hex (j + x1, k + y1). Same
    for the second point *)
let hex_vertex_overlap =
  [
    (0, (2, 0, -1), (4, 1, -1));
    (1, (3, 1, -1), (5, 1, 0));
    (2, (4, 1, 0), (0, 0, 1));
    (3, (1, -1, 1), (5, 0, 1));
    (4, (2, -1, 0), (0, -1, 1));
    (5, (3, 0, -1), (1, -1, 0));
  ]

(** All the relative coordinates of overlaps between hexes and their edges. For
    example: the element [(e, (e1, x, y))] indicates that at hex (j, k), edge e
    is the same as edge e1 of hex (j + x1, k + y1). *)
let hex_edge_overlap =
  [
    (0, (3, 1, -1));
    (1, (4, 1, 0));
    (2, (5, 0, 1));
    (3, (0, -1, 1));
    (4, (1, -1, 0));
    (5, (2, 0, -1));
  ]

let set_get_color_test (name : string) (s : int) (coord : point)
    (input_color : color) (expected_output : color) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (s |> from_size |> Hex.set_color input_color coord |> Hex.get_color coord)

let set_get_number_test (name : string) (s : int) (coord : point)
    (input_number : int option) (expected_output : int option) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (s |> from_size |> set_number input_number coord |> get_number coord)

let set_get_building_test (name : string) (building : building) (p : point)
    (v : int) (t : Hex.t) (point_to_get : point) (vertex_to_get : int)
    (expected_output : building) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (t |> set_building building p v |> get_building point_to_get vertex_to_get)

let set_get_road_test (name : string) (road : road) (p : point) (e : int)
    (t : Hex.t) (point_to_get : point) (edge_to_get : int)
    (expected_output : road) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (t |> set_road road p e |> get_road point_to_get edge_to_get)

(** [board_check s] creates a board of size [s] and checks that each [tile] in
    the returned [tile list] has a [color] that is [Red], [Green], [Brown],
    [Desert], or [Grey] and a [number] within 2..12, inclusive. Returns [true]
    if this is the case, and [false] otherwise. *)
let board_check (s : int) : bool =
  let board : Hex.t = make_board s in
  let coord_list = coordinates board in
  let check_tile pt =
    let tile_color = Hex.get_color pt board in
    let tile_num =
      match get_number pt board with
      | Some x -> x
      | None -> 0
    in
    (tile_color = Desert && tile_num = 0)
    || (tile_color <> None && tile_num <= 12 && tile_num >= 2)
  in
  List.for_all check_tile coord_list

(** [player_test test_name id p_name expected_id expected_name] constructs an
    OUnit test case of name [test_name] that asserts the equality of the [name]
    and [id] of a player constructed by calling [init_player id p_name] with
    [expected_id] and [expected_name], respectively. *)
let init_player_test (name : string) (id : int) (p_name : string)
    (expected_id : int) (expected_name : string) : test =
  name >:: fun _ ->
  assert_equal
    (expected_id, expected_name)
    (let p = init_player id p_name in
     (player_id p, player_name p))

let resources_test (name : string) (p : Player.t) (resource_type : resource)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (num_resource p resource_type)

(** [make_board_tests] runs a QCheck test that checks [make_board_test] *)
let make_board_tests =
  [
    (* check board of random size *)
    QCheck.Test.make ~count:1 QCheck.int board_check
    |> QCheck_runner.to_ounit2_test;
    (* check board of size 3 (normal gameboard size). *)
    (let board_check' _ = board_check 3 in
     QCheck.Test.make ~count:1000 QCheck.int board_check'
     |> QCheck_runner.to_ounit2_test);
  ]

let size_tests =
  [
    size_test "Size of from_size 0 should be 1" 0 1;
    size_test "Size of from_size 1 should be 7" 1 7;
    size_test "Size of from_size 2 should be 19" 2 19;
  ]

let coordinates_tests =
  [
    coordinates_test "Only valid coord of from_size 0 is (0,0)" 0 [ (0, 0) ];
    coordinates_test "from_size 1" 1
      [ (0, 0); (~-1, 1); (1, ~-1); (~-1, 0); (0, ~-1); (1, 0); (0, 1) ];
  ]

let set_get_color_tests =
  [
    set_get_color_test "from_size 0" 0 (0, 0) (Resource Player.Brick)
      (Resource Player.Brick);
    set_get_color_test "from_size 1" 1 (1, -1) (Resource Player.Brick)
      (Resource Player.Brick);
    set_get_color_test "None" 1 (1, -1) None None;
  ]

let set_get_number_tests =
  [
    set_get_number_test "from_size 0" 0 (0, 0) (Some 1) (Some 1);
    set_get_number_test "from_size 1" 1 (1, -1) (Some 1) (Some 1);
    set_get_color_test "None" 1 (1, -1) None None;
  ]

let init_player_tests =
  [ init_player_test "player with id 0 and name \"\"" 0 "" 0 "" ]

let res_ex =
  Invalid_argument "Cannot remove more of a resource than the player has"

let resources_tests =
  [
    (* Test Player.num_resources on empty player *)
    resources_test "no resources added, Brick" (init_player 0 "") Brick 2;
    resources_test "no resources added, Metal" (init_player 0 "") Metal 2;
    resources_test "no resources added, Ore" (init_player 0 "") Ore 2;
    resources_test "no resources added, Livestock" (init_player 0 "") Livestock
      2;
    resources_test "no resources added, Lumber" (init_player 0 "") Lumber 2
    (* Test Player.add_resources *);
    resources_test "0 Brick added, check Brick"
      (let p = init_player 0 "" in
       add_resource p Brick 0;
       p)
      Brick 2;
    resources_test "1 Lumber added, check Lumber"
      (let p = init_player 0 "" in
       add_resource p Lumber 1;
       p)
      Lumber 3;
    resources_test "1 Ore added, check Livestock"
      (let p = init_player 0 "" in
       add_resource p Ore 1;
       p)
      Livestock 2;
    resources_test "add multiple of one"
      (let p = init_player 0 "" in
       add_resource p Brick 2;
       p)
      Brick 4;
    resources_test "add same resource multiple times"
      (let p = init_player 0 "" in
       add_resource p Lumber 1;
       add_resource p Lumber 2;
       add_resource p Lumber 0;
       p)
      Lumber 5;
    resources_test "add same resource multiple times"
      (let p = init_player 0 "" in
       add_resource p Lumber 2;
       add_resource p Lumber 2;
       add_resource p Lumber 0;
       p)
      Lumber 6;
    resources_test "add negative resource"
      (let p = init_player 0 "" in
       add_resource p Livestock 3;
       add_resource p Livestock (-1);
       p)
      Livestock 4;
    ( "remove unadded resource throws exception" >:: fun _ ->
      assert_raises res_ex (fun () ->
          let p = init_player 0 "" in
          add_resource p Brick ~-4) );
    ( "remove too much of added resource throws exception" >:: fun _ ->
      assert_raises res_ex (fun () ->
          let p = init_player 0 "" in
          add_resource p Livestock 3;
          add_resource p Livestock (-7);
          p) );
  ]

let set_get_building_test_lst =
  [
    set_get_building_test "simple from_size 0 empty tile" EmptyBuilding (0, 0) 0
      (from_size 0) (0, 0) 0 EmptyBuilding;
    set_get_building_test "simple from_size 0 adding 1 town"
      (Town (init_player 0 ""))
      (0, 0) 0 (from_size 0) (0, 0) 0
      (Town (init_player 0 ""));
    set_get_building_test "simple from_size 0 check other empty vertex"
      (Town (init_player 0 ""))
      (0, 0) 0 (from_size 0) (0, 0) 1 EmptyBuilding;
    set_get_building_test "from_size 1 check original vertex set"
      (Town (init_player 0 ""))
      (0, 0) 0 (from_size 1) (0, 0) 0
      (Town (init_player 0 ""));
    set_get_building_test "from_size 1 check other identical vertex (1/2)"
      (Town (init_player 0 ""))
      (0, 0) 0 (from_size 1) (0, -1) 2
      (Town (init_player 0 ""));
    set_get_building_test "from_size 1 check other identical vertex (2/2)"
      (Town (init_player 0 ""))
      (0, 0) 0 (from_size 1) (1, -1) 4
      (Town (init_player 0 ""));
  ]

(** A list that tests every overlapping vertex from the origin *)
let set_get_all_identical_vertex_building_test =
  let h = from_size 1 in
  let p = init_player 0 "test_player" in
  let t = Town p in
  List.fold_left
    (fun lst (v, (v1, x1, y1), (v2, x2, y2)) ->
      set_get_building_test
        ("relative vertex first overlap of vertex " ^ string_of_int v)
        t (0, 0) v h (x1, y1) v1 t
      :: set_get_building_test
           ("relative vertex second overlap of vertex " ^ string_of_int v)
           t (0, 0) v h (x2, y2) v2 t
      :: lst)
    [] hex_vertex_overlap

(** A list that tests every overlapping edge from the origin *)
let set_get_all_identical_edge_road_test =
  let h = from_size 1 in
  let p = init_player 0 "test_player" in
  let t = Road p in
  List.map
    (fun (e, (e1, x1, y1)) ->
      set_get_road_test
        ("relative edge overlap of edge " ^ string_of_int e)
        t (0, 0) e h (x1, y1) e1 t)
    hex_edge_overlap

let test_get_set_throws_exception name ex f index =
  name >:: fun _ -> assert_raises ex (fun () -> from_size 2 |> f (-1, 1) index)

(** Test exceptions are thrown for getting/setting vertex/edges *)
let get_vertex_edge_test_exception =
  [
    test_get_set_throws_exception "negative get edge throws exception"
      EdgeOutOfBounds get_road ~-1;
    test_get_set_throws_exception "too large get edge throws exception"
      EdgeOutOfBounds get_road 7;
    test_get_set_throws_exception "too large get vertex throws exception"
      VertexOutOfBounds get_building 7;
    test_get_set_throws_exception "negative get vertex throws exception"
      VertexOutOfBounds get_building ~-7;
    test_get_set_throws_exception "negative set edge throws exception"
      EdgeOutOfBounds (set_road EmptyRoad) ~-7;
    test_get_set_throws_exception "too large set edge throws exception"
      EdgeOutOfBounds (set_road EmptyRoad) 7;
    test_get_set_throws_exception "negative set building throws exception"
      VertexOutOfBounds
      (set_building EmptyBuilding)
      ~-7;
    test_get_set_throws_exception "too large set building throws exception"
      VertexOutOfBounds
      (set_building EmptyBuilding)
      7;
  ]

let set_get_road_test =
  [
    set_get_road_test "simple from_size 0 empty tile" EmptyRoad (0, 0) 0
      (from_size 0) (0, 0) 0 EmptyRoad;
    set_get_road_test "simple from_size 0 adding 1 road"
      (Road (init_player 0 ""))
      (0, 0) 0 (from_size 0) (0, 0) 0
      (Road (init_player 0 ""));
    set_get_road_test "simple from_size 0 check other empty vertex"
      (Road (init_player 0 ""))
      (0, 0) 0 (from_size 0) (0, 0) 1 EmptyRoad;
    set_get_road_test "from_size 1 check original edge set"
      (Road (init_player 0 ""))
      (0, 0) 0 (from_size 1) (0, 0) 0
      (Road (init_player 0 ""));
    set_get_road_test "from_size 1 check other identical edge"
      (Road (init_player 0 ""))
      (0, 0) 0 (from_size 1) (1, -1) 3
      (Road (init_player 0 ""));
  ]

(** [game_objects_test name expected_output input] constructs an OUnit testcase
    that asserts the equality of [expected_output] with the output of [input]. *)
let game_objects_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input

(** [create_test_game n t] is a test game with [n] players, with [t] turns
    advanced. *)
let create_test_game n t =
  let names = [| "A"; "B"; "C"; "D" |] in
  let new_game = create_new_game () in
  (* Add players *)
  Array.iteri (fun i name -> if i < n then add_new_player new_game name) names;
  (* Advance turns *)
  for i = 1 to t do
    next_turn new_game
  done;
  new_game

let game_objects_tests =
  [
    game_objects_test "number of players in empty game" 0
      (num_players (create_test_game 0 0));
    game_objects_test "number of players in a game with 1 player" 1
      (create_test_game 1 0 |> num_players);
    game_objects_test "number of players in a three player game" 3
      (create_test_game 3 0 |> num_players);
    game_objects_test "number of players in 2 player game" 2
      (create_test_game 2 0 |> num_players);
    ( "adding too many players" >:: fun _ ->
      assert_raises PlayerLimit (fun () ->
          let new_game = create_test_game 4 0 in
          add_new_player new_game "E") );
    game_objects_test "next once on a two player game" "B"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test "next multiple times on two player game" "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       next_turn new_game;
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test "next twice on a one player game" "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       next_turn new_game;
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test "next multiple times on a one player game" "A"
      (let new_game = create_test_game 1 7 in
       player_name (current_player new_game));
    game_objects_test "next on three person game" "B"
      (let new_game = create_test_game 3 4 in
       player_name (current_player new_game));
    ( "getting a player that doesn't exist" >:: fun _ ->
      assert_raises PlayerNotFound (fun () ->
          let new_game = create_new_game () in
          add_new_player new_game "A";
          get_player new_game 1) );
    game_objects_test "adding a player and then getting that player" "A"
      (let new_game = create_test_game 1 0 in
       player_name (get_player new_game 0));
    game_objects_test "getting second player" "B"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       player_name (get_player new_game 1));
    game_objects_test "getting first player in two player game with get_player"
      "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       player_name (get_player new_game 0));
    game_objects_test "getting first player in three player game" "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       add_new_player new_game "C";
       player_name (get_player new_game 0));
    game_objects_test "getting second player in three player game" "B"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       add_new_player new_game "C";
       player_name (get_player new_game 1));
    game_objects_test "getting third player in three player game" "C"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       add_new_player new_game "C";
       player_name (get_player new_game 2));
    game_objects_test "getting current player after adding one player" "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       player_name (current_player new_game));
    game_objects_test "getting current player after adding two players" "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       player_name (current_player new_game));
    game_objects_test "getting next player after adding a player" "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test
      "getting next player multiple times after adding a\n\n      player" "A"
      (let new_game = create_test_game 1 5 in
       player_name (current_player new_game));
    game_objects_test "getting next player after adding two players" "B"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test "getting next player after\n      adding two players" "A"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       next_turn new_game;
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test
      "number of players after removing player from no\n      players" 0
      (let new_game = create_new_game () in
       let player_a = init_player 0 "A" in
       remove_player new_game player_a;
       num_players new_game);
    game_objects_test
      "number of players after adding and removing\n      a player" 0
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       let player_a = get_player new_game 0 in
       remove_player new_game player_a;
       num_players new_game);
    game_objects_test "removing first player\n      from game" "B"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       let player_a = get_player new_game 0 in
       remove_player new_game player_a;
       player_name (current_player new_game));
    game_objects_test
      "next player after removing what would be\n      next player" "C"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       add_new_player new_game "C";
       let player_b = get_player new_game 1 in
       remove_player new_game player_b;
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test "removing next player before going to next turn" "C"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       let player_b = get_player new_game 1 in
       add_new_player new_game "C";
       remove_player new_game player_b;
       next_turn new_game;
       player_name (current_player new_game));
    game_objects_test "getting third player after removing it in list of 4" "D"
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       add_new_player new_game "C";
       add_new_player new_game "D";
       let player_c = get_player new_game 2 in
       remove_player new_game player_c;
       player_name (get_player new_game 3));
    ( "getting a player that is removed raises an error" >:: fun _ ->
      assert_raises PlayerNotFound (fun () ->
          let new_game = create_new_game () in
          add_new_player new_game "A";
          let player_a = get_player new_game 0 in
          remove_player new_game player_a;
          get_player new_game 0) );
    game_objects_test "get_all_players when zero players."
      (Array.make 0 (init_player 0 "A"))
      (let new_game = create_new_game () in
       get_all_players new_game);
    (let new_game = create_new_game () in
     add_new_player new_game "A";
     let player_a = get_player new_game 0 in
     game_objects_test "get all\n      players of one player"
       (Array.make 1 player_a) (get_all_players new_game));
    (let new_game = create_new_game () in
     add_new_player new_game "A";
     let player_a = get_player new_game 0 in
     remove_player new_game player_a;
     game_objects_test "get all players of adding and removing a\n      player"
       (Array.make 0 player_a) (get_all_players new_game));
    (let new_game = create_new_game () in
     add_new_player new_game "A";
     add_new_player new_game "B";
     add_new_player new_game "C";
     add_new_player new_game "D";
     let player_c = get_player new_game 2 in
     let player_a = get_player new_game 0 in
     let player_b = get_player new_game 1 in
     let player_d = get_player new_game 3 in
     game_objects_test "get all players of game of four players"
       [| player_a; player_b; player_c; player_d |]
       (get_all_players new_game));
    (let new_game = create_new_game () in
     add_new_player new_game "A";
     add_new_player new_game "B";
     add_new_player new_game "C";
     add_new_player new_game "D";
     let player_c = get_player new_game 2 in
     let player_a = get_player new_game 0 in
     let player_b = get_player new_game 1 in
     let player_d = get_player new_game 3 in
     remove_player new_game player_b;
     add_new_player new_game "E";
     let player_e = get_player new_game 4 in
     game_objects_test
       "get all players of game of four players after removing one and adding  \
        new one"
       [| player_a; player_c; player_d; player_e |]
       (get_all_players new_game));
  ]

let roll_tests =
  let create_test_roll_game () =
    let game = create_test_game 1 0 in
    Hex.from_size 3
    |> Hex.set_building (Town (current_player game)) (0, 0) 0
    |> Hex.set_building (City (current_player game)) (1, -1) 2
    |> Hex.set_number (Some 6) (0, 0)
    |> Hex.set_number (Some 8) (1, -1)
    |> Hex.set_number (Some 8) (1, 0)
    |> Hex.set_color (Resource Brick) (0, 0)
    (* Test branch when number matches, but no color *)
    |> Hex.set_color None (2, 0)
    |> Hex.set_number (Some 6) (2, 0)
    |> Hex.set_color (Resource Lumber) (1, -1)
    |> Hex.set_color (Resource Lumber) (1, 0)
    |> set_map game;
    game
  in
  let open Game.Gameobjects in
  [
    ( "roll on empty board" >:: fun _ ->
      assert_equal 2
        (let game = create_test_game 1 0 in
         Gameobjects.set_map game (Hex.from_size 3);
         execute_specific_roll 3 3 game |> ignore;
         num_resource (game |> current_player) Brick) );
    ( "roll returns correct values" >:: fun _ ->
      assert_equal (4, 5)
        (execute_specific_roll 4 5 (Gameobjects.create_new_game ()) |> values)
    );
    ( "roll selects town" >:: fun _ ->
      assert_equal ~printer:string_of_int 3
        (let game = create_test_roll_game () in
         execute_specific_roll 3 3 game |> ignore;
         num_resource (current_player game) Brick) );
    ( "roll selects city; city overlaps two selected tiles" >:: fun _ ->
      assert_equal ~printer:string_of_int
        7 (* 5 b/c two cities and one town, start w 2 *)
        (let game = create_test_roll_game () in
         execute_specific_roll 4 4 game |> ignore;
         num_resource (current_player game) Lumber) );
    ( "many rolls on default board (test for no exceptions)" >:: fun _ ->
      let game = create_test_game 4 4 in
      for i = 0 to 20 do
        next_turn game;
        execute_roll game |> ignore
      done );
  ]

(** [x -- y] is a list enumerated from x to y (inclusive).

    Source: OCaml Programming: Correct + Efficient + Beautiful by Michael R.
    Clarkson et al. (2022)*)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

let add_resources player =
  let resources = [ Brick; Metal; Ore; Livestock; Lumber ] in
  List.iter (fun r -> add_resource player r 50) resources
(* add_resource player Brick 10; *)
(* | Metal | Ore | Livestock | Lumber *)

let action_test_game () =
  let names = [| "A"; "B"; "C"; "D" |] in
  let new_game = create_new_game () in
  (* Add players *)
  Array.iter (fun name -> add_new_player new_game name) names;
  let ids = 0 -- (Array.length names - 1) in
  (* Add some initial resources*)
  List.iter
    (fun id ->
      let p = get_player new_game id in
      add_resources p)
    ids;
  new_game

let take_actions () =
  let g = action_test_game () in
  let p = get_player g 0 in
  take_action g (new_vertex_action p (Hex.Town p) (0, 0) 0);
  take_action g (new_vertex_action p (Hex.Town p) (0, 0) 1);
  take_action g (new_vertex_action p (Hex.City p) (0, 0) 1);
  take_action g (new_edge_action p (Hex.Road p) (0, 0) 0);
  g

let g = take_actions ()
let g_board = get_map g
let p = get_player g 0

let create_test (name : string) expected_output actual_output =
  name >:: fun _ -> assert_equal expected_output actual_output

let take_action_tests =
  [
    create_test "correct road count" 1 (num_structure p RoadStructure);
    create_test "correct town count" 1 (num_structure p TownStructure);
    create_test "correct city count" 1 (num_structure p CityStructure);
    create_test "correct Brick count" 49 (num_resource p Brick);
    create_test "correct Metal count" 48 (num_resource p Metal);
    create_test "correct Ore count" 49 (num_resource p Ore);
    create_test "correct Livestock count" 50 (num_resource p Livestock);
    create_test "correct Lumber count" 49 (num_resource p Lumber);
    create_test "correct points" 3 (num_points p);
    create_test "board updated with road in correct position"
      (get_road (0, 0) 0 g_board)
      (Road p);
    create_test "board updated with town in correct position"
      (get_building (0, 0) 0 g_board)
      (Town p);
    create_test "board updated with city in correct position"
      (get_building (0, 0) 1 g_board)
      (City p);
  ]

let take_actions_validate_1 () =
  let g = action_test_game () in
  let p1 = get_player g 0 in
  let p2 = get_player g 1 in
  let p3 = get_player g 2 in
  let p4 = get_player g 3 in
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 0);
  take_action g (new_vertex_action p2 (Hex.Town p2) (0, 0) 2);
  take_action g (new_edge_action p1 (Hex.Road p1) (0, -2) 2);
  take_action g (new_edge_action p1 (Hex.Road p1) (0, -1) 2);
  take_action g (new_edge_action p1 (Hex.Road p1) (2, -1) 5);
  take_action g (new_edge_action p1 (Hex.Road p1) (2, -1) 4);
  take_action g (new_edge_action p1 (Hex.Road p1) (3, 0) 1);
  take_action g (new_edge_action p2 (Hex.Road p2) (0, 0) 2);
  take_action g (new_edge_action p2 (Hex.Road p2) (1, -1) 0);
  take_action g (new_edge_action p2 (Hex.Road p2) (-1, 1) 4);
  take_action g (new_edge_action p2 (Hex.Road p2) (0, -1) 0);
  take_action g (new_edge_action p3 (Hex.Road p3) (1, -2) 4);
  take_action g (new_edge_action p4 (Hex.Road p4) (-2, 3) 0);
  g

(*needed to overcome max player limit*)
let take_actions_validate_2 () =
  let g = action_test_game () in
  let p1 = get_player g 0 in
  take_action g (new_edge_action p1 (Hex.Road p1) (-3, 0) 0);
  g

let deplete_resources player =
  let resources = [ Brick; Metal; Ore; Livestock; Lumber ] in
  List.iter
    (fun r -> add_resource player r (-1 * num_resource player r))
    resources;
  player

let inflate_structure_counts player =
  let structures = [ RoadStructure; TownStructure; CityStructure ] in
  List.iter (fun r -> add_structure player r 100) structures;
  player

let g1 = take_actions_validate_1 ()
let g2 = take_actions_validate_2 ()
let g_board1 = get_map g1
let g_board2 = get_map g2
let p1 = get_player g1 0
let p2 = get_player g1 1
let p3 = get_player g1 2
let p4 = get_player g1 3
let p4 = deplete_resources p4

(*needed to overcome max player limit*)
let p1_v2 = get_player g2 0
let p1_v2 = inflate_structure_counts p1_v2

let validate_action_tests =
  [
    create_test "correct road" true
      (validate_action g1 (new_edge_action p1 (Hex.Road p1) (0, 0) 0));
    create_test "coordinates need to be valid" false
      (validate_action g1 (new_edge_action p1 (Hex.Road p1) (10, 10) 0));
    create_test "vertex/edge needs to be valid" false
      (validate_action g1 (new_edge_action p1 (Hex.Road p1) (0, 0) 7));
    create_test "new road needs to belong to player" false
      (validate_action g1 (new_edge_action p1 EmptyRoad (0, 0) 0));
    create_test "need enough resources for road" false
      (validate_action g1 (new_edge_action p4 (Hex.Road p4) (-2, 3) 1));
    create_test "can't have more than max number of roads" false
      (validate_action g2 (new_edge_action p1_v2 (Hex.Road p1_v2) (-3, 0) 1));
    create_test "new road needs to connect to existing road belonging to player"
      false
      (validate_action g1 (new_edge_action p1 (Hex.Road p1) (-3, 2) 0));
    create_test "correct town next to one of player's roads " true
      (validate_action g1 (new_vertex_action p1 (Hex.Town p1) (2, -1) 0));
    create_test "correct town in between two of player's roads" true
      (validate_action g1 (new_vertex_action p1 (Hex.Town p1) (2, -1) 5));
    create_test
      "vertex in the middle of three different player's roads for player 1" true
      (validate_action g1 (new_vertex_action p1 (Hex.Town p1) (0, -1) 0));
    create_test
      "vertex in the middle of three different player's roads for player 2" true
      (validate_action g1 (new_vertex_action p2 (Hex.Town p2) (0, -1) 0));
    create_test
      "vertex in the middle of three different player's roads for player 3" true
      (validate_action g1 (new_vertex_action p3 (Hex.Town p3) (0, -1) 0));
    create_test "distance rule violation" false
      (validate_action g1 (new_vertex_action p1 (Hex.Town p1) (0, 0) 5));
    create_test "building needs to belong to player" false
      (validate_action g1 (new_vertex_action p1 EmptyBuilding (2, -1) 0));
    create_test
      "new building needs to connect to existing road belonging to player" false
      (validate_action g1 (new_vertex_action p1 (Hex.Town p1) (-3, 2) 0));
    create_test "need enough resources for building" false
      (validate_action g1 (new_vertex_action p4 (Hex.Town p4) (-2, 3) 1));
    create_test "can't have more than max number of buildings" false
      (validate_action g2 (new_vertex_action p1_v2 (Hex.Town p1_v2) (-3, 0) 1));
    create_test
      "even there is a road connected, can't place if both other roads \
       connecting to vertex belong to another player"
      false
      (validate_action g1 (new_vertex_action p2 (Hex.Town p2) (2, -1) 5));
    create_test "correct city where player's town currently is" true
      (validate_action g1 (new_vertex_action p1 (Hex.City p1) (0, 0) 0));
    create_test "incorrect city" false
      (validate_action g1 (new_vertex_action p1 (Hex.City p1) (0, 0) 5));
    create_test "correct coast town" true
      (validate_action g1 (new_vertex_action p1 (Hex.Town p1) (3, 0) 1));
  ]

let test_board = from_size 1
let test_board2 = from_size 2

let canonical_vertex_tests =
  [
    create_test "everything at (0, 0)" true
      (List.fold_left
         (fun acc (v, (v1, x1, y1), (v2, x2, y2)) ->
           acc
           && canonical_vertex (0, 0) v test_board
              = canonical_vertex (x1, y1) v1 test_board
           && canonical_vertex (0, 0) v test_board
              = canonical_vertex (x2, y2) v2 test_board)
         true hex_vertex_overlap);
    create_test "edge case with two valid vertices" true
      (canonical_vertex (1, -1) 2 test_board
      = canonical_vertex (1, 0) 0 test_board);
    create_test "edge case with one valid vertex"
      ((1, -1), 1)
      (canonical_vertex (1, -1) 1 test_board);
    create_test "edge case with two valid vertices" true
      (canonical_vertex (1, -2) 5 test_board2
      = canonical_vertex (0, -2) 1 test_board2);
  ]

let canonical_edge_tests =
  [
    create_test "everything at (0, 0)" true
      (List.fold_left
         (fun acc (e, (e1, x1, y1)) ->
           acc
           && canonical_edge (0, 0) e test_board
              = canonical_edge (x1, y1) e1 test_board)
         true hex_edge_overlap);
    create_test "edge case with two valid edges" true
      (canonical_edge (1, -1) 2 test_board = canonical_edge (1, 0) 5 test_board);
    create_test "edge case with one valid vertex"
      ((1, -1), 1)
      (canonical_edge (1, -1) 1 test_board);
  ]

let no_winner_game () =
  let g = action_test_game () in
  let p1 = get_player g 0 in
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 0);
  take_action g (new_vertex_action p1 (Hex.City p1) (0, 0) 0);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 2);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 3);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 4);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 5);
  take_action g (new_vertex_action p1 (Hex.Town p1) (1, 0) 0);
  g

let winner_game () =
  let g = action_test_game () in
  let p1 = get_player g 0 in
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 0);
  take_action g (new_vertex_action p1 (Hex.City p1) (0, 0) 0);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 2);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 3);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 4);
  take_action g (new_vertex_action p1 (Hex.Town p1) (0, 0) 5);
  take_action g (new_vertex_action p1 (Hex.Town p1) (1, 0) 0);
  take_action g (new_vertex_action p1 (Hex.Town p1) (1, 0) 1);
  take_action g (new_vertex_action p1 (Hex.Town p1) (1, 0) 2);
  take_action g (new_vertex_action p1 (Hex.City p1) (1, 0) 2);
  g

let g1 = no_winner_game ()
let g2 = winner_game ()
let p1 = get_player g2 0

let check_winner_tests =
  [
    create_test "no winner" Option.None (check_winner g1);
    create_test "winner" (Option.Some p1) (check_winner g2);
  ]

let robber_tests =
  [
    create_test "needs to move robber" true
      (let new_game = create_new_game () in
       ignore (execute_specific_roll 1 6 new_game);
       needs_move_robber new_game);
    create_test "does not need to move robber" false
      (let new_game = create_new_game () in
       ignore (execute_specific_roll 2 6 new_game);
       needs_move_robber new_game);
    create_test "move robber, check current location" (0, 0)
      (let new_game = create_new_game () in
       move_robber new_game (0, 0);
       curr_loc_robber new_game);
    create_test "move robber, check needs move" (true, false)
      (let new_game = create_new_game () in
       ignore (execute_specific_roll 1 6 new_game);
       let c1 = needs_move_robber new_game in
       move_robber new_game (0, 0);
       let c2 = needs_move_robber new_game in
       (c1, c2));
  ]

let rand_game_tests =
  [
    create_test "two player colors not the same" true
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       add_new_player new_game "B";
       add_new_player new_game "C";
       let player_A_clr = get_color (get_player new_game 0) in
       let player_B_clr = get_color (get_player new_game 1) in
       let player_C_clr = get_color (get_player new_game 2) in
       player_A_clr <> player_B_clr
       && player_A_clr <> player_C_clr
       && player_B_clr <> player_C_clr);
    create_test
      "testing start_player, number of roads, towns, and victory points"
      (1, 1, 1)
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       let player_A = get_player new_game 0 in
       start_player new_game player_A;
       ( num_structure player_A RoadStructure,
         num_structure player_A TownStructure,
         num_points player_A ));
    create_test "testing give_player_resources" true
      (let new_game = create_new_game () in
       add_new_player new_game "A";
       give_player_resources new_game;
       let player_A = get_player new_game 0 in
       num_resource player_A Brick >= 2
       && num_resource player_A Metal >= 2
       && num_resource player_A Ore >= 2
       && num_resource player_A Livestock >= 2
       && num_resource player_A Lumber >= 2);
  ]

let suite =
  "test suite"
  >::: List.flatten
         [
           size_tests;
           coordinates_tests;
           set_get_color_tests;
           set_get_number_tests;
           make_board_tests;
           init_player_tests;
           resources_tests;
           set_get_building_test_lst;
           set_get_road_test;
           set_get_all_identical_vertex_building_test;
           set_get_all_identical_edge_road_test;
           get_vertex_edge_test_exception;
           game_objects_tests;
           roll_tests;
           take_action_tests;
           validate_action_tests;
           canonical_vertex_tests;
           canonical_edge_tests;
           check_winner_tests;
           robber_tests;
           rand_game_tests;
         ]

let _ = run_test_tt_main suite
