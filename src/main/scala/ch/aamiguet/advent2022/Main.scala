package ch.aamiguet
package advent2022

@main def Main(args: String*): Unit =
  println("─" * 100)

  val d = Day22("input/day22.txt")

  println(d.solvePart1)
  println(d.solvePart2)

  println("─" * 100)
