package ch.aamiguet
package advent2022

@main def Main(args: String*): Unit =
  println("─" * 100)

  val d = Day9("input/day9.txt")

  println(d.solvePart1)
  println(d.solvePart2)
  d.visualize

  println("─" * 100)
