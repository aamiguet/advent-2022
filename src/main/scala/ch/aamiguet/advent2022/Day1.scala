package ch.aamiguet.advent2022

import scala.io.Source

case class Day1(
    filename: String
  ):
  final case class Elf(
      id: Int,
      calories: Int,
    )

  lazy val lines = Source.fromFile(filename).getLines().toList

  lazy val elfes: List[Elf] =
    lines.foldLeft(List.empty[Elf]) { (acc, line) =>
      if (line == "")
        Elf(acc.head.id + 1, 0) :: acc
      else if (acc.isEmpty)
        List(Elf(1, line.toInt))
      else
        acc.head.copy(calories = acc.head.calories + line.toInt) :: acc.tail
    }

  def solvePart1: Int =
    elfes.map(_.calories).max

  def solvePart2: Int =
    elfes.sortWith(_.calories > _.calories).take(3).map(_.calories).sum

