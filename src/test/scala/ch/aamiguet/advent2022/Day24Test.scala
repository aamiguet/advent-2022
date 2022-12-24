package ch.aamiguet.advent2022

final class Day24Test extends TestSuite:
  val d = Day24("input/test24.txt")

  test("day 24") {
    expect(
      d.solvePart1 === 18,
      d.solvePart2 === 54,
    )
  }
