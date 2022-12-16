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

  def parseLine(line: String): (Sensor, Beacon) =
    val regex = """(-)?\d+""".r
    val coords = regex.findAllIn(line).toArray.map(_.toInt)
    val beacon = Beacon(coords(2), coords(3))
    val sensor =
      Sensor(coords(0), coords(1), Math.abs(beacon.x - coords(0)) + Math.abs(beacon.y - coords(1)))
    (sensor, beacon)

  lazy val lines = Source.fromFile(filename).getLines().filter(_.nonEmpty).toList
  lazy val (sensors, beacons) = lines.foldLeft((List.empty[Sensor], Set.empty[Beacon])) {
    (acc, line) =>
      val (sensor, beacon) = parseLine(line)
      (sensor :: acc._1, acc._2 + beacon)
  }

  def coveredPositions(row: Int): Int =
    val relevantSensors = sensors.filter(s => Math.abs(row - s.y) <= s.distance)
    val cps = scala.collection.mutable.Set.empty[Int]
    relevantSensors.foreach(s =>
      val delta = s.distance - Math.abs(row - s.y)
      val r = s.x - delta to s.x + delta
      cps ++= r
    )
    cps.size - beacons.filter(_.y == row).size

  // test if the position is seen by any sensor
  def seen(x: Int, y: Int) =
    sensors.foldLeft(false) { (acc, s) =>
      acc || {
        s.distance >= Math.abs(y - s.y) + Math.abs(x - s.x)
      }
    }

  // all positions at max distance + 1 from a sensor
  def candidates(sensor: Sensor, limit: Int): List[Beacon] =
    val border = sensor.distance + 1
    (for {
      y <- sensor.y - border to sensor.y + border if y >= 0 && y <= limit
      delta <- List(border - Math.abs(y - sensor.y))
      x <- List(sensor.x - delta, sensor.x + delta) if x >= 0 && x <= limit
    } yield (Beacon(x, y))).toList

  // for each sensor, test all positions that are at max distance + 1 if it seen by any sensor
  // end when a position is found that is not seen by any sensor
  // a bit slow but fast enough!
  def tuningFrequency(limit: Int): Long =
    val distress = sensors.foldLeft[Option[Beacon]](None) { (acc, s) =>
      acc match
        case Some(_) => acc
        case None =>
          val cands = candidates(s, limit)
          cands.find(b => !seen(b.x, b.y))
    }
    distress match
      case Some(d) => d.x * 4000000L + d.y
      case None => throw new Exception("No distress beacon found")

  def solvePart1: Int =
    coveredPositions(2000000)

  def solvePart2: Long =
    tuningFrequency(4000000)
