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

  def exposedSides(pos: Position) =
    neighbours(pos).filterNot(cubes.contains).size

  def solvePart1: Int =
    cubes.toList.map(exposedSides).sum
