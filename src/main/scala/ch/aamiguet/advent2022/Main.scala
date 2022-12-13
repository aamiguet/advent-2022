package ch.aamiguet
package advent2022

@main def Main(args: String*): Unit =
  println("─" * 100)

  val d = Day13("input/day13.txt")

  println(d.solvePart1)
  println(d.solvePart2)

  println("─" * 100)
