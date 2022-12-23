package ch.aamiguet
package advent2022

@main def Main(args: String*): Unit =
  println("─" * 100)

  val d = Day21("input/day21.txt")

  println(d.solvePart1)
  println(d.solvePart2)

  println("─" * 100)
