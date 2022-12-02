package ch.aamiguet
package advent2022

@main def Main(args: String*): Unit =
  println("─" * 100)

  val d = Day2("input/day2.txt")

  println(d.solvePart1)
  println(d.solvePart2)

  println("─" * 100)
