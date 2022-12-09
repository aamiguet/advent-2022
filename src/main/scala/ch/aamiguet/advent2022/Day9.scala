package ch.aamiguet.advent2022

import scala.io.Source

final case class Day9(
    filename: String
  ):
  enum Move:
    case Up, Right, Down, Left

  final case class Position(x: Int, y: Int)
  lazy val lines = Source.fromFile(filename).getLines().filter(_.nonEmpty).toList

  lazy val moves: List[Move] = lines.flatMap { l =>
    val arr = l.split(" ")
    val d = arr(1).toInt
    arr(0) match
      case "U" => List.fill(d)(Move.Up)
      case "R" => List.fill(d)(Move.Right)
      case "D" => List.fill(d)(Move.Down)
      case "L" => List.fill(d)(Move.Left)
  }

  def step(c1: Int, c2: Int): Int =
    if c1 > c2 then -1
    else 1

  def nextHead(head: Position, move: Move): Position =
    move match
      case Move.Up => Position(head.x, head.y + 1)
      case Move.Right => Position(head.x + 1, head.y)
      case Move.Down => Position(head.x, head.y - 1)
      case Move.Left => Position(head.x - 1, head.y)

  def nextPosition(knot: Position, precedingKnot: Position): Position =
    val xDistance = Math.abs(knot.x - precedingKnot.x)
    val yDistance = Math.abs(knot.y - precedingKnot.y)

    if xDistance <= 1 && yDistance <= 1 then knot
    else if xDistance != 0 && yDistance != 0 then
      knot.copy(
        x = knot.x + step(knot.x, precedingKnot.x),
        y = knot.y + step(knot.y, precedingKnot.y),
      )
    else if xDistance == 0 && yDistance > 1 then
      knot.copy(y = knot.y + step(knot.y, precedingKnot.y))
    else if yDistance == 0 && xDistance > 1 then
      knot.copy(x = knot.x + step(knot.x, precedingKnot.x))
    else knot

  def computeMotions(
      headPath: List[Position],
      tailPath: List[Position],
      moves: List[Move],
    ): (List[Position], List[Position]) =
    if moves.isEmpty then (headPath, tailPath)
    else
      val m = moves.head
      val h = headPath.head
      val t = tailPath.head
      val nextH = nextHead(h, m)
      val nextT = nextPosition(t, nextH)

      computeMotions(nextH :: headPath, nextT :: tailPath, moves.tail)

  def computeMotions(knots: List[List[Position]], moves: List[Move]): List[List[Position]] =
    if moves.isEmpty then knots
    else
      val m = moves.head
      val nextH = nextHead(knots.head.head, m)
      val newPos = knots
        .tail
        .foldLeft(List(nextH)) { (acc, knot) =>
          val nextKnot = nextPosition(knot.head, acc.head)
          nextKnot :: acc
        }
        .reverse
      val newKnots = newPos.zip(knots).map { (k, ks) =>
        k :: ks
      }
      computeMotions(newKnots, moves.tail)

  def solvePart1: Int =
    val (headPath, tailPath) = computeMotions(List(Position(0, 0)), List(Position(0, 0)), moves)
    tailPath.toSet.size
  def solvePart2: Int =
    val knots = computeMotions(List.fill(10)(List(Position(0, 0))), moves)
    knots.last.toSet.size
