package ch.aamiguet.advent2022

import scala.io.Source

final case class Day15(
    filename: String
  ):
  case class Sensor(
      x: Int,
      y: Int,
      distance: Int,
    )
  case class Beacon(
      x: Int,
      y: Int,
    )

  /*object Sensor:
    val regex = """(-)?\d+""".r
    def apply(line: String): Sensor =
      val coords = regex.findAllIn(line).toArray.map(_.toInt)
      val (x, y) = (coords(0), coords(1))
      val dist = Math.abs(coords(2) - x) + Math.abs(coords(3) - y)
      Sensor(x, y, dist)*/

  def parseLine(line: String): (Sensor, Beacon) =
    val regex = """(-)?\d+""".r
    val coords = regex.findAllIn(line).toArray.map(_.toInt)
    val beacon = Beacon(coords(2), coords(3))
    val sensor = Sensor(coords(0), coords(1), Math.abs(beacon.x - coords(0)) + Math.abs(beacon.y - coords(1)))
    (sensor, beacon)

  lazy val lines = Source.fromFile(filename).getLines().filter(_.nonEmpty).toList
  lazy val (sensors, beacons) = lines.foldLeft((List.empty[Sensor], Set.empty[Beacon])) { (acc, line) =>
    val (sensor, beacon) = parseLine(line)
    ((sensor :: acc._1), acc._2 + beacon)
  }

  def coveredPositions(row: Int): Int =
    val relevantSensors = sensors.filter(s => Math.abs(row - s.y) <= s.distance)

    val xs = relevantSensors.foldLeft(Set.empty[Int]){(acc, s) =>
      val delta = s.distance - Math.abs(row - s.y)
      val r = s.x - delta to s.x + delta
      acc.union(r.toSet)
    }
    xs.size - beacons.filter(_.y == row).size

  def solvePart1: Int =
    coveredPositions(2000000)

  def solvePart2: Int = ???
