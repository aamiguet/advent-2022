package ch.aamiguet.advent2022

import scala.io.Source

final case class Day22(
    filename: String
  ):
  trait Action
  case class Move(n: Int) extends Action

  enum Turn:
    case TurnLeft, TurnRight

  trait Facing:
    def value: Int
    def change(turn: Turn): Facing

  case object Right extends Facing:
    val value = 0
    def change(turn: Turn): Facing = turn match
      case Turn.TurnLeft => Up
      case Turn.TurnRight => Down

  case object Down extends Facing:
    val value = 1
    def change(turn: Turn): Facing = turn match
      case Turn.TurnLeft => Right
      case Turn.TurnRight => Left

  case object Left extends Facing:
    val value = 2
    def change(turn: Turn): Facing = turn match
      case Turn.TurnLeft => Down
      case Turn.TurnRight => Up

  case object Up extends Facing:
    val value = 3
    def change(turn: Turn): Facing = turn match
      case Turn.TurnLeft => Left
      case Turn.TurnRight => Right

  case class Position(
      x: Int,
      y: Int,
    )

  enum Tile:
    case Open, Wall, Void

  lazy val (grid, path) =
    val lines = Source.fromFile(filename).getLines().toList
    val width = lines.takeWhile(_.nonEmpty).sortWith(_.length > _.length).head.length
    val g = lines
      .takeWhile(_.nonEmpty)
      .zipWithIndex
      .flatMap {
        case (line, y) =>
          val arr = line.toCharArray
          (0 until width).map { x =>
            val tile = arr.lift(x) match
              case Some('#') => Tile.Wall
              case Some('.') => Tile.Open
              case _ => Tile.Void
            Position(x, y) -> tile
          }
      }
      .toMap
    (g, lines.last)
  lazy val maxX = grid.keys.map(_.x).max
  lazy val maxY = grid.keys.map(_.y).max

  def nextPhysicalPosition(position: Position, facing: Facing): Position =
    val next = facing match
      case Right =>
        if position.x + 1 > maxX then Position(0, position.y)
        else Position(position.x + 1, position.y)
      case Down =>
        if position.y + 1 > maxY then Position(position.x, 0)
        else Position(position.x, position.y + 1)
      case Left =>
        if position.x - 1 < 0 then Position(maxX, position.y)
        else Position(position.x - 1, position.y)
      case Up =>
        if position.y - 1 < 0 then Position(position.x, maxY)
        else Position(position.x, position.y - 1)
    if grid.get(next).get == Tile.Void then nextPhysicalPosition(next, facing) else next

  def nextPosition(
      position: Position,
      facing: Facing,
      step: Int,
    ): Position =
    if step == 0 then position
    else
      val next = nextPhysicalPosition(position, facing)
      grid.get(next).get match
        case Tile.Open => nextPosition(next, facing, step - 1)
        case Tile.Wall => position
        case _ => throw Error("Unexpected tile")

  def finalPosition(
      position: Position,
      facing: Facing,
      path: String,
    ): (Position, Facing) =
    if path.isEmpty then (position, facing)
    else
      path.head match
        case 'R' => finalPosition(position, facing.change(Turn.TurnRight), path.tail)
        case 'L' => finalPosition(position, facing.change(Turn.TurnLeft), path.tail)
        case _ =>
          val (n, rest) = path.span(_.isDigit)
          finalPosition(nextPosition(position, facing, n.toInt), facing, rest)

  def nextPhysicalPositionInCube(position: Position, facing: Facing): (Position, Facing) =
    if rules.isDefinedAt((position, facing)) then rules((position, facing))
    else
      val p = facing match
        case Right =>
          if position.x + 1 > maxX then Position(0, position.y)
          else Position(position.x + 1, position.y)
        case Down =>
          if position.y + 1 > maxY then Position(position.x, 0)
          else Position(position.x, position.y + 1)
        case Left =>
          if position.x - 1 < 0 then Position(maxX, position.y)
          else Position(position.x - 1, position.y)
        case Up =>
          if position.y - 1 < 0 then Position(position.x, maxY)
          else Position(position.x, position.y - 1)
      (p, facing)

  def nextPositionInCube(
      position: Position,
      facing: Facing,
      step: Int,
    ): (Position, Facing) =
    if step == 0 then (position, facing)
    else
      val (p, f) = nextPhysicalPositionInCube(position, facing)
      grid.get(p).get match
        case Tile.Open => nextPositionInCube(p, f, step - 1)
        case Tile.Wall => (position, facing)
        case _ => throw Error("Unexpected tile")

  def finalPositionInCube(
      position: Position,
      facing: Facing,
      path: String,
    ): (Position, Facing) =
    if path.isEmpty then (position, facing)
    else
      path.head match
        case 'R' => finalPositionInCube(position, facing.change(Turn.TurnRight), path.tail)
        case 'L' => finalPositionInCube(position, facing.change(Turn.TurnLeft), path.tail)
        case _ =>
          val (n, rest) = path.span(_.isDigit)
          val (p, f) = nextPositionInCube(position, facing, n.toInt)
          finalPositionInCube(p, f, rest)

  def password(position: Position, facing: Facing): Int =
    (position.y + 1) * 1000 + (position.x + 1) * 4 + facing.value

  def solvePart1: Int =
    val startingPos =
      (0 to maxX).find(x => grid(Position(x, 0)) == Tile.Open).map(Position(_, 0)).get
    val (finalPos, finalFacing) = finalPosition(startingPos, Right, path)
    password(finalPos, finalFacing)

  def solvePart2: Int =
    val startingPos =
      (0 to maxX).find(x => grid(Position(x, 0)) == Tile.Open).map(Position(_, 0)).get
    val (finalPos, finalFacing) = finalPositionInCube(startingPos, Right, path)
    password(finalPos, finalFacing)

  // these wrapping rules are fitting my input
  val rules: Map[(Position, Facing), (Position, Facing)] =
    (0 to 49).flatMap { x =>
      Map(
        (Position(x, 100), Up) -> (Position(50, 50 + x), Right),
        (Position(50, 50 + x), Left) -> (Position(x, 100), Down),
        (Position(x, 199), Down) -> (Position(100 + x, 0), Down),
        (Position(100 + x, 0), Up) -> (Position(x, 199), Up),
      )
    }.toMap ++
      (50 to 99).flatMap { x =>
        Map(
          (Position(x, 0), Up) -> (Position(0, x + 100), Right),
          (Position(0, x + 100), Left) -> (Position(x, 0), Down),
          (Position(x, 149), Down) -> (Position(49, x + 100), Left),
          (Position(49, x + 100), Right) -> (Position(x, 149), Up),
        )
      } ++
      (100 to 149).flatMap { x =>
        Map(
          (Position(x, 49), Down) -> (Position(99, x - 50), Left),
          (Position(99, x - 50), Right) -> (Position(x, 49), Up),
        )
      } ++
      (0 to 49).flatMap { y =>
        Map(
          (Position(50, y), Left) -> (Position(0, 149 - y), Right),
          (Position(0, 149 - y), Left) -> (Position(50, y), Right),
          (Position(149, y), Right) -> (Position(99, 149 - y), Left),
          (Position(99, 149 - y), Right) -> (Position(149, y), Left),
        )

      }

  // these wrapping rules are fitting the test input
  val testRules: Map[(Position, Facing), (Position, Facing)] =
    (4 to 7).flatMap { x =>
      Map(
        (Position(x, 4), Up) -> (Position(8, x - 4), Right),
        (Position(8, x - 4), Left) -> (Position(x, 4), Down),
        (Position(x, 7), Down) -> (Position(8, 15 - x), Right),
        (Position(8, 15 - x), Left) -> (Position(x, 7), Up),
      )
    }.toMap ++
      (0 to 3).flatMap { x =>
        Map(
          (Position(x, 4), Up) -> (Position(11 - x, 0), Down),
          (Position(11 - x, 0), Up) -> (Position(x, 4), Down),
          (Position(x, 7), Down) -> (Position(11 - x, 11), Up),
          (Position(11 - x, 11), Down) -> (Position(x, 7), Up),
        )
      } ++
      (12 to 15).flatMap { x =>
        Map(
          (Position(x, 8), Up) -> (Position(11, 19 - x), Left),
          (Position(11, 19 - x), Right) -> (Position(x, 8), Down),
          (Position(x, 11), Down) -> (Position(0, 19 - x), Right),
          (Position(0, 19 - x), Left) -> (Position(x, 11), Up),
        )
      } ++
      (8 to 11).flatMap { y =>
        Map(
          (Position(15, y), Right) -> (Position(11, 11 - y), Left),
          (Position(11, 11 - y), Right) -> (Position(15, y), Left),
        )
      }
