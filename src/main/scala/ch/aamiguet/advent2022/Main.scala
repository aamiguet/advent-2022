package ch.aamiguet
package advent2022

@main def Main(args: String*): Unit =
  println("─" * 100)

  val d = Day10("input/day10.txt")

  println(d.solvePart1)
  d.solvePart2

  println("─" * 100)
