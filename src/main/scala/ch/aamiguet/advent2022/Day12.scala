package ch.aamiguet.advent2022

import scala.io.Source

case class Day12(
    filename: String
  ):
  case class Position(
      x: Int,
      y: Int,
    ):
    def neighbours(maxX: Int, maxY: Int): List[Position] =
      List(
        Position(x - 1, y),
        Position(x + 1, y),
        Position(x, y - 1),
        Position(x, y + 1),
      ).filter(p => p.x >= 0 && p.x <= maxX && p.y >= 0 && p.y <= maxY)

  lazy val lines = Source.fromFile(filename).getLines().filter(_.nonEmpty)
  lazy val heightmap =
    lines.map(_.toCharArray).toArray
  lazy val altitude =
    heightmap.map(_.map {
      case 'E' => 'z'.toInt
      case 'S' => 'a'.toInt
      case c => c.toInt
    })

  def position(c: Char): Position =
    val y = heightmap.indexWhere(_.contains(c))
    val x = heightmap(y).indexOf(c)
    Position(x, y)

  lazy val start = position('S')
  lazy val end = position('E')
  lazy val maxX = altitude.head.size - 1
  lazy val maxY = altitude.size - 1

  def findPaths(paths: List[List[Position]], visited: Set[Position]): List[List[Position]] =
    if visited.contains(end) || visited.size == altitude.size * altitude.head.size then paths
    else
      val vs = visited.to(collection.mutable.Set)
      val newPaths = paths.flatMap { path =>
        val pos = path.head
        val accessibleNeighbours = pos
          .neighbours(maxX, maxY)
          .filterNot(vs.contains)
          .filter(p => altitude(p.y)(p.x) <= altitude(pos.y)(pos.x) + 1)
        vs ++= accessibleNeighbours
        accessibleNeighbours.map(p => p :: path)
      }
      findPaths(newPaths, vs.to(Set))

  def allPaths(pos: Position): List[List[Position]] = findPaths(List(List(pos)), Set(pos))

  def pathLength(pos: Position): Int =
    val paths = allPaths(pos)
    paths.find(_.head == end).get.size - 1

  def solvePart1: Int = pathLength(start)

  def solvePart2: Int =
    val starts = (for {
      x <- 0 to maxX
      y <- 0 to maxY if altitude(y)(x) == 'a'
    } yield Position(x, y)).toList
    val paths = findPaths(starts.map(List(_)), starts.toSet)
    paths.find(_.head == end).get.size - 1
