package ch.aamiguet.advent2022

import scala.io.Source

final case class Day4(
    filename: String
  ):
  lazy val lines = Source.fromFile(filename).getLines().toList.filter(_.nonEmpty)

  def range(s: String): Range =
    val Array(min, max) = s.split("-").map(_.toInt)
    min to max

  def fullyOverlaps(line: String): Boolean =
    val s = line.split(",").map(range)
    s(0).intersect(s(1)) == s(0) || s(1).intersect(s(0)) == s(1)

  def partiallyOverlaps(line: String): Boolean =
    val s = line.split(",").map(range)
    s(0).intersect(s(1)).nonEmpty

  def solvePart1: Int =
    lines.filter(fullyOverlaps).size
  def solvePart2: Int =
    lines.filter(partiallyOverlaps).size
