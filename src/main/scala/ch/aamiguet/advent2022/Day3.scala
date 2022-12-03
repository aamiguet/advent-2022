package ch.aamiguet.advent2022

import scala.io.Source

final case class Day3(
  filename: String
):

  lazy val lines = Source.fromFile(filename).getLines().toList.filter(_.nonEmpty)

  extension(itemType: Char)
    def score: Int = if (itemType.isLower) itemType.toInt - 96 else itemType.toInt - 38

  def lineScore(line: String): Int =
    val (l, r) = line.splitAt(line.length / 2)
    val intersection = l.filter(r.contains(_))
    intersection.head.score

  def groupScore(group: List[String]): Int =
    val intersection = group.head.filter(group.tail.head.contains(_)).filter(group.tail.tail.head.contains(_))
    intersection.head.score

  def solvePart1: Int = lines.map(lineScore).sum
  def solvePart2: Int = lines.grouped(3).map(groupScore).sum