(** Constants for the game rendering. For example: positions of various objects
    and colors *)

(** The size of the screen in x. *)
let screen_width = 750

(** The size of the screen in y. *)
let screen_height = 600

(** Ratio of hex size to circle inside *)
let number_size_ratio = 0.5

(** Default radius to draw the hexes. *)
let radius = 30

(** The name of the window (appears in the title bar) *)
let window_title = "Settlers of Cornell"

(** Colors for everything displayed. *)
module Colors = struct
  (** Circles surrounding number labels. *)
  let interior_circle = (245, 218, 191)

  (** Scene background. *)
  let bg = (222, 246, 255)

  (** Default color of a button *)
  let button_default = (0, 0, 0)

  (** Borders around hex tiles *)
  let hex_border_color = (50, 50, 50)

  (** Color for brick *)
  let brick = (161, 3, 78)

  (** Color for livestock *)
  let livestock = (176, 212, 135)

  (** Color for lumber *)
  let lumber = (59, 98, 3)

  (** Color for ore *)
  let ore = (136, 67, 0)

  (** Color for metal *)
  let metal = (135, 135, 135)

  (** Color for desert *)
  let desert = (242, 214, 170)

  (** Color for an empty tile (white) *)
  let empty_tile = (255, 255, 255)

  (** Color for background of player's hand *)
  let hand_background = (82, 56, 30)
end

(** Positional data needed for drawing a rectangle in floats *)
module type PositionFloat = sig
  val widthf : float
  (** width of object *)

  val heightf : float
  (** height of object *)

  val xf : float
  (** x location of object *)

  val yf : float
  (** y location of object *)
end

(** Full positional data needed for drawing a rectangle, with both floats and
    ints *)
module type Position = sig
  val widthf : float
  (** width of object *)

  val heightf : float
  (** height of object *)

  val xf : float
  (** x location of object *)

  val yf : float
  (** y location of object *)

  val height : int
  (** height of object (truncated to int) *)

  val width : int
  (** width of object (truncated to int) *)

  val x : int
  (** x position of object (truncated to int) *)

  val y : int
  (** x position of object (truncated to int) *)

  val pos : int * int
  (** position of object as pair (truncated to int) *)

  val size : int * int
  (** size of object (truncated to int) *)

  val centerf : float * float
  (** exact center of object *)

  val center : int * int
  (** center of object (truncated to int) *)
end

(** Preform additional calculations to convert a module with only float values
    to a complete Position *)
module ToInt (M : PositionFloat) : Position = struct
  include M

  let height = int_of_float M.heightf
  let width = int_of_float M.widthf
  let x = int_of_float M.xf
  let y = int_of_float M.yf
  let pos = (x, y)
  let size = (width, height)
  let centerf = (xf +. (widthf /. 2.), yf +. (heightf /. 2.))

  let center =
    let a, b = centerf in
    (int_of_float a, int_of_float b)
end

(** Position of current player's hand on screen *)
module Hand = ToInt (struct
  let widthf = float_of_int screen_width *. 0.75
  let heightf = float_of_int screen_height *. 0.22
  let xf = (float_of_int screen_width /. 2.) -. (widthf /. 2.)
  let yf = float_of_int screen_height -. heightf
end)

(** Position of button for player to roll *)
module RollButton = ToInt (struct
  let widthf = 0.7 *. Hand.heightf
  let heightf = widthf
  let xf = Hand.xf +. (0.15 *. Hand.heightf)
  let yf = Hand.yf +. (0.15 *. Hand.heightf)
end)

(** Position of button for player to advance to next turn *)
module NextTurn = ToInt (struct
  let widthf = 0.7 *. Hand.heightf
  let heightf = widthf
  let xf = Hand.xf +. Hand.widthf -. widthf -. (0.15 *. Hand.heightf)
  let yf = Hand.yf +. (0.15 *. Hand.heightf)
end)

(** Position of button for player to enter building mode *)
module BuildTown = ToInt (struct
  let widthf = 0.7 *. Hand.heightf
  let heightf = widthf /. 2.
  let xf = NextTurn.xf -. widthf -. (0.15 *. Hand.heightf)
  let yf = Hand.yf +. (0.15 *. Hand.heightf)
end)

(** Position of dice 1 *)
module Dice1 = ToInt (struct
  let widthf = 50.
  let heightf = 50.
  let xf = Hand.xf -. widthf -. 15.
  let yf = Hand.yf +. 10.
end)

(** Position of dice 2 *)
module Dice2 = ToInt (struct
  let widthf = 50.
  let heightf = 50.
  let xf = Dice1.xf
  let yf = Dice1.yf +. Dice1.heightf +. 10.
end)

(** Position of full screen button *)
module StartButton = ToInt (struct
  let widthf = float_of_int screen_width
  let heightf = float_of_int screen_height
  let xf = 0.
  let yf = 0.
end)

(** Position to display the current number of resources that a player has *)
module type ResourceLabelType = sig
  include Position

  val c : Pgraphics.color
  (** Color of the resource *)

  val r : Game.Player.resource
  (** Resource type *)
end

(** Contains all the resource labels *)
module ResourceLabels = struct
  (** Metal resource label *)
  module Metal : ResourceLabelType = struct
    include ToInt (struct
      let widthf = BuildTown.heightf
      let heightf = BuildTown.heightf
      let xf = RollButton.widthf +. RollButton.xf +. (0.15 *. Hand.heightf)
      let yf = BuildTown.yf
    end)

    let c = Colors.metal
    let r = Game.Player.Metal
  end

  (** Lumber resource label *)
  module Lumber : ResourceLabelType = struct
    include ToInt (struct
      let widthf = Metal.widthf
      let heightf = Metal.heightf
      let xf = Metal.xf +. Metal.widthf
      let yf = Metal.yf
    end)

    let c = Colors.lumber
    let r = Game.Player.Lumber
  end

  (** Brick resource label *)
  module Brick : ResourceLabelType = struct
    include ToInt (struct
      let widthf = BuildTown.heightf
      let heightf = BuildTown.heightf
      let xf = RollButton.widthf +. RollButton.xf +. (0.15 *. Hand.heightf)
      let yf = BuildTown.yf +. BuildTown.heightf
    end)

    let c = Colors.brick
    let r = Game.Player.Brick
  end

  (** Livestock resource label *)
  module Livestock : ResourceLabelType = struct
    include ToInt (struct
      let widthf = Metal.widthf
      let heightf = Metal.heightf
      let xf = Metal.xf +. Metal.widthf
      let yf = Metal.yf +. Metal.heightf
    end)

    let c = Colors.livestock
    let r = Game.Player.Livestock
  end

  (** Ore resource label *)
  module Ore : ResourceLabelType = struct
    include ToInt (struct
      let widthf = Lumber.widthf
      let heightf = Lumber.heightf
      let xf = Lumber.xf +. Lumber.widthf
      let yf = Lumber.yf +. (Lumber.heightf /. 2.)
    end)

    let c = Colors.ore
    let r = Game.Player.Ore
  end
end

(** Options for performance profiling *)
module ProfileOptions = struct
  (** Set to [true] to enable performance profiling *)
  let enabled = false

  (** Set to [true] to display profiling results to console *)
  let display = false

  (** Set to [true] to display debug info in the console *)
  let debug_print = false
end
