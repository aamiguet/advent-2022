package ch.aamiguet.advent2022

import scala.io.Source

final case class Day24(
    filename: String
  ):
  case class Position(
      x: Int,
      y: Int,
    )

  enum Direction:
    case Up, Right, Down, Left

  case class Blizzard(
      position: Position,
      direction: Direction,
    ):
    def move(
        maxX: Int,
        maxY: Int,
      ): Blizzard =
      val newPosition = direction match
        case Direction.Up =>
          if position.y == 0 then Position(position.x, maxY)
          else Position(position.x, position.y - 1)
        case Direction.Right =>
          if position.x == maxX then Position(0, position.y)
          else Position(position.x + 1, position.y)
        case Direction.Down =>
          if position.y == maxY then Position(position.x, 0)
          else Position(position.x, position.y + 1)
        case Direction.Left =>
          if position.x == 0 then Position(maxX, position.y)
          else Position(position.x - 1, position.y)
      copy(position = newPosition)

  lazy val grid: Map[Position, List[Blizzard]] =
    val m = scala.collection.mutable.Map.empty[Position, List[Blizzard]]
    val lines = Source.fromFile(filename).getLines().toList.filter(_.nonEmpty)
    lines.drop(1).dropRight(1).zipWithIndex.foreach { (line, y) =>
      line.drop(1).dropRight(1).zipWithIndex.foreach { (c, x) =>
        val position = Position(x, y)
        val blizzard = c match
          case '>' => Some(Blizzard(position, Direction.Right))
          case '<' => Some(Blizzard(position, Direction.Left))
          case '^' => Some(Blizzard(position, Direction.Up))
          case 'v' => Some(Blizzard(position, Direction.Down))
          case _ => None
        blizzard.foreach(b => m.update(b.position, b :: m.getOrElse(b.position, Nil)))
      }
    }
    m.toMap

  lazy val maxX = grid.keys.map(_.x).max
  lazy val maxY = grid.keys.map(_.y).max
  lazy val startingPos = Position(0, -1)
  lazy val finalPos = Position(maxX, maxY + 1)

  def nextGrid(grid: Map[Position, List[Blizzard]]): Map[Position, List[Blizzard]] =
    val m = scala.collection.mutable.Map.empty[Position, List[Blizzard]]
    grid.foreach { (_, blizzards) =>
      blizzards.foreach { b =>
        val movedBlizzard = b.move(maxX, maxY)
        m.update(movedBlizzard.position, movedBlizzard :: m.getOrElse(movedBlizzard.position, Nil))
      }
    }
    m.toMap

  def movablePosition(position: Position): Set[Position] =
    Set(
      Position(position.x, position.y - 1),
      Position(position.x + 1, position.y),
      Position(position.x, position.y + 1),
      Position(position.x - 1, position.y),
      position,
    ).filter(p =>
      p == startingPos || p == finalPos || (p.x >= 0 && p.y >= 0 && p.x <= maxX && p.y <= maxY)
    )

  def legalPositions(position: Position, grid: Map[Position, List[Blizzard]]): Set[Position] =
    movablePosition(position).filter(p => !grid.isDefinedAt(p))

  def computeRound(
      positions: Set[Position],
      grid: Map[Position, List[Blizzard]],
      round: Int,
      target: Position,
    ): (Int, Map[Position, List[Blizzard]]) =
    if positions.contains(target) then (round, grid)
    else
      val ng = nextGrid(grid)
      val nextPositions = positions.flatMap(p => legalPositions(p, ng))
      computeRound(nextPositions, ng, round + 1, target)

  def solvePart1: Int =
    computeRound(Set(startingPos), grid, 0, finalPos)._1

  def solvePart2: Int =
    val firstTrip = computeRound(Set(startingPos), grid, 0, finalPos)
    val secondTrip = computeRound(Set(finalPos), firstTrip._2, 0, startingPos)
    val thirdTrip = computeRound(Set(startingPos), secondTrip._2, 0, finalPos)
    thirdTrip._1 + secondTrip._1 + firstTrip._1
