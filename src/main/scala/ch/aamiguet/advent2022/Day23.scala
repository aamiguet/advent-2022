package ch.aamiguet.advent2022

import scala.io.Source

final case class Day23(
    filename: String
  ):
  case class Position(
      x: Int,
      y: Int,
    )

  trait Direction:
    def next: Direction
    def positions(position: Position): List[Position]
    def proposedPosition(position: Position): Position

  case object North extends Direction:
    def next = South
    def positions(position: Position) = List(
      Position(position.x - 1, position.y + 1),
      Position(position.x, position.y + 1),
      Position(position.x + 1, position.y + 1),
    )
    def proposedPosition(position: Position): Position = Position(position.x, position.y + 1)

  case object South extends Direction:
    def next = West
    def positions(position: Position) = List(
      Position(position.x - 1, position.y - 1),
      Position(position.x, position.y - 1),
      Position(position.x + 1, position.y - 1),
    )
    def proposedPosition(position: Position): Position = Position(position.x, position.y - 1)

  case object West extends Direction:
    def next = East
    def positions(position: Position) = List(
      Position(position.x - 1, position.y - 1),
      Position(position.x - 1, position.y),
      Position(position.x - 1, position.y + 1),
    )
    def proposedPosition(position: Position): Position = Position(position.x - 1, position.y)

  case object East extends Direction:
    def next = North
    def positions(position: Position) = List(
      Position(position.x + 1, position.y - 1),
      Position(position.x + 1, position.y),
      Position(position.x + 1, position.y + 1),
    )
    def proposedPosition(position: Position): Position = Position(position.x + 1, position.y)

  def proposedPosition(
      startingDirection: Direction,
      position: Position,
      elves: Set[Position],
    ): Option[Position] =
    def loop(direction: Direction): Option[Position] =
      if direction.positions(position).forall(!elves.contains(_)) then
        Some(direction.proposedPosition(position))
      else if direction.next == startingDirection then None
      else loop(direction.next)
    loop(startingDirection)

  def isAlone(
    position: Position,
    elves: Set[Position],
  ): Boolean =
    val positions = List(
      Position(position.x - 1, position.y - 1),
      Position(position.x - 1, position.y),
      Position(position.x - 1, position.y + 1),
      Position(position.x, position.y - 1),
      Position(position.x, position.y + 1),
      Position(position.x + 1, position.y - 1),
      Position(position.x + 1, position.y),
      Position(position.x + 1, position.y + 1),
    )
    positions.forall(!elves.contains(_))

  def proposedPosition(
      startingDirection: Direction,
      elves: Set[Position],
    ): Map[Position, Option[Position]] =
    elves.map { elf =>
      if isAlone(elf, elves) then elf -> None
      else elf -> proposedPosition(startingDirection, elf, elves)
    }.toMap

  def move(
      elves: Set[Position],
      proposedPositions: Map[Position, Option[Position]],
    ): Set[Position] =
    val excludedPosition = proposedPositions
      .values
      .collect { case Some(p) => p }
      .groupBy(identity)
      .filter(_._2.size > 1)
      .keySet
    elves.map { elf =>
      proposedPositions(elf) match
        case None => elf
        case Some(p) if excludedPosition.contains(p) => elf
        case Some(p) => p
    }

  def emptyGround(elves: Set[Position]): Int =
    val xs = elves.map(_.x)
    val ys = elves.map(_.y)
    (xs.max - xs.min + 1) * (ys.max - ys.min + 1) - elves.size

  def printMap(elves: Set[Position], n: Int): Unit =
    val xs = elves.map(_.x)
    val ys = elves.map(_.y)
    println("-" * 48 + s" $n " + "-" * 48)
    for y <- (ys.min to ys.max).reverse do
      for x <- xs.min to xs.max do
        if elves.contains(Position(x, y)) then print("#")
        else print(".")
      println()
    println("-" * 100)

  def round(elves: Set[Position], direction: Direction, n: Int): Set[Position] =
    if n == 0 then elves
    else
      val proposedPositions = proposedPosition(direction, elves)
      val newElves = move(elves, proposedPositions)
      round(newElves, direction.next, n - 1)

  def stoppingRound(elves: Set[Position], direction: Direction, n: Int): Int =
    val proposedPositions = proposedPosition(direction, elves)
    if proposedPositions.values.forall(_.isEmpty) then n
    else
      val newElves = move(elves, proposedPositions)
      stoppingRound(newElves, direction.next, n + 1)

  lazy val elves =
    val lines = Source.fromFile(filename).getLines().toList.filter(_.nonEmpty).reverse
    lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.collect { case ('#', x) => Position(x, y) }
    }.toSet

  def solvePart1: Int = emptyGround(round(elves, North, 10))
  def solvePart2: Int = stoppingRound(elves, North, 1)
