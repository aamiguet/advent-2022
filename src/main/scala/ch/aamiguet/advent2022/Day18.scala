package ch.aamiguet.advent2022

import scala.io.Source

final case class Day18(
    filename: String
  ):
  case class Position(
      x: Int,
      y: Int,
      z: Int,
    )

  lazy val cubes = Source
    .fromFile(filename)
    .getLines()
    .toList
    .filter(_.nonEmpty)
    .map { line =>
      val arr = line.split(",")
      Position(arr(0).toInt, arr(1).toInt, arr(2).toInt)
    }
    .toSet

  def neighbours(pos: Position) =
    List(
      Position(pos.x - 1, pos.y, pos.z),
      Position(pos.x + 1, pos.y, pos.z),
      Position(pos.x, pos.y - 1, pos.z),
      Position(pos.x, pos.y + 1, pos.z),
      Position(pos.x, pos.y, pos.z - 1),
      Position(pos.x, pos.y, pos.z + 1),
    )

  def exposedSides(pos: Position, cubes: Set[Position]) =
    neighbours(pos).filterNot(cubes.contains).size

  def exposedSides(cubes: Set[Position]): Int =
    cubes.toList.map(exposedSides(_, cubes)).sum

  def solvePart1: Int =
    exposedSides(cubes)

  def pocketOfAir(
      cubes: List[Position],
      cubesOfAir: List[Position],
      acc: Set[Position],
    ): Set[Position] =
    if cubes.isEmpty then acc
    else
      val cube = cubes.head
      val matchingNeighbours = neighbours(cube)
        .filterNot(cubes.contains)
        .filterNot(acc.contains)
        .filter(cubesOfAir.contains)
      pocketOfAir(cubes.tail ++ matchingNeighbours, cubesOfAir, acc ++ matchingNeighbours)

  def pocketsOfAir(
      cubesOfAir: List[Position],
      acc: List[Set[Position]],
    ): List[Set[Position]] =
    if cubesOfAir.isEmpty then acc
    else
      val cube = cubesOfAir.head
      val pocket = pocketOfAir(List(cube), cubesOfAir.tail, Set(cube))
      pocketsOfAir(cubesOfAir.filterNot(pocket.contains), pocket :: acc)

  def solvePart2: Int =
    val (xs, ys, zs) = cubes.map(c => (c.x, c.y, c.z)).unzip3
    val allCubes =
      for {
        x <- xs.min - 1 to xs.max + 1
        y <- ys.min - 1 to ys.max + 1
        z <- zs.min - 1 to zs.max + 1
      } yield Position(x, y, z)
    val cubesOfAir = allCubes.filterNot(cubes.contains).toList
    val pockets = pocketsOfAir(cubesOfAir, List.empty)

    // inner pockets are pockets that are inside the initial cube, so we remove the large pocket around the lava droplet
    val innerPockets = pockets.filterNot(p => p.contains(Position(xs.min - 1, ys.min - 1, zs.min - 1)))
    val innerSurface = innerPockets.map(exposedSides).sum

    exposedSides(cubes) - innerSurface
