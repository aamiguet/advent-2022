package ch.aamiguet.advent2022

import scala.io.Source

final case class Day17(
    filename: String
  ):

  case class Position(
      x: Int,
      y: Long,
    )

  trait Rock:
    def next: Rock
    def currentPos(position: Position): Map[Position, Char]

    def isLegal(position: Position, tower: Map[Position, Char]): Boolean =
      val c = currentPos(position)
      val keys = c.keys
      keys.forall:
        p => p.x >= 0 && p.x < 7 && !tower.isDefinedAt(p)

    def afterJetPos(position: Position, jet: Char, tower: Map[Position, Char]): Position =
      val newPos = jet match
        case '<' => Position(position.x - 1, position.y)
        case '>' => Position(position.x + 1, position.y)
      if isLegal(newPos, tower) then  newPos else position

    def afterGravityPos(position: Position, tower: Map[Position, Char]): Position =
      val newPos  = Position(position.x, position.y - 1)
      if isLegal(newPos, tower) then newPos else position

  case object HorizontalLine extends Rock:
    def next = Cross
    def currentPos(position: Position) =
      Map(
        Position(position.x, position.y) -> '#',
        Position(position.x + 1, position.y) -> '#',
        Position(position.x + 2, position.y) -> '#',
        Position(position.x + 3, position.y) -> '#',
      )

  case object Cross extends Rock:
    def next = Angle
    def currentPos(position: Position) =
      Map(
        Position(position.x + 1, position.y) -> '#',
        Position(position.x, position.y + 1) -> '#',
        Position(position.x + 1, position.y + 1) -> '#',
        Position(position.x + 2, position.y + 1) -> '#',
        Position(position.x + 1, position.y + 2) -> '#',
      )

  case object Angle extends Rock:
    def next = VerticalLine
    def currentPos(position: Position) =
      Map(
        Position(position.x, position.y) -> '#',
        Position(position.x + 1, position.y) -> '#',
        Position(position.x + 2, position.y) -> '#',
        Position(position.x + 2, position.y + 1) -> '#',
        Position(position.x + 2, position.y + 2) -> '#',
      )
  case object VerticalLine extends Rock:
    def next = Square
    def currentPos(position: Position) =
      Map(
        Position(position.x, position.y) -> '#',
        Position(position.x, position.y + 1) -> '#',
        Position(position.x, position.y + 2) -> '#',
        Position(position.x, position.y + 3) -> '#',
      )

  case object Square extends Rock:
    def next = HorizontalLine
    def currentPos(position: Position) =
      Map(
        Position(position.x, position.y) -> '#',
        Position(position.x + 1, position.y) -> '#',
        Position(position.x, position.y + 1) -> '#',
        Position(position.x + 1, position.y + 1) -> '#',
      )

  lazy val jetPattern = Source.fromFile(filename).getLines().mkString("").toCharArray
  lazy val jetPatternSize = jetPattern.size

  def jet(n: Int): Char =
    jetPattern(n)

  def towerHeight(tower: Map[Position, Char]): Long =
    tower.keys.map(_.y).max

  def reduceAcc(acc: Map[Position, Char]): Map[Position, Char] =
    val h = towerHeight(acc)
    acc.filter(_._1.y >= h - 40)

  def tower(rock: Rock, jetIndex: Int, acc: Map[Position, Char], n: Long): Map[Position, Char] =
    if n == 0 then acc
    else
      var pos = Position(2, towerHeight(acc) + 4)
      var j = jetIndex
      var isStuck = false
      while !isStuck do
        if j == 0 then
          println(s"n = $n with $rock at $pos")
        val ajPos = rock.afterJetPos(pos, jet(j), acc)
        val agPos = rock.afterGravityPos(ajPos, acc)
        isStuck = ajPos == agPos
        pos = agPos
        j = (j + 1) % jetPatternSize
      if n == 999999998262L then
        tower(rock.next, j, reduceAcc(acc ++ rock.currentPos(pos)), 999999998262L % 1730 - 1)
      else
      tower(rock.next, j, reduceAcc(acc ++ rock.currentPos(pos)), n - 1)

  def printTower(tower: Map[Position, Char]): Unit =
    val height = towerHeight(tower)
    val width = tower.keys.map(_.x).max
    for y <- (0L to height).reverse do
      for x <- 0 to width do
        print(tower.getOrElse(Position(x, y), '.'))
      println()

  def tower(n: Long): Map[Position, Char] =
    val t = (0 to 6).map (i => Position(i, 0) -> '_').toMap
    tower(HorizontalLine, 0, t, n)

  def solvePart1: Long =
    val finalTower = tower(2022)
    towerHeight(finalTower)

  def solvePart2: Long =
    val finalTower = tower(1000000000000L)
    // for my input, I observe that there is a repeating pattern every 1730 iterations
    // this pattern has a height of 2644
    // so we can skip a bunch of iterations
    // the better solution would be to automate the detection of the pattern ;-)
    towerHeight(finalTower) + (999999998262L/1730L) * 2644L

