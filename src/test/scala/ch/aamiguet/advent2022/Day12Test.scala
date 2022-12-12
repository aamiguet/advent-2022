package ch.aamiguet.advent2022

final class Day12Test extends TestSuite:
  val d = Day12("input/test12.txt")

  test("day 12") {
    expect(
      d.solvePart1 === 31,
      d.solvePart2 === 29,
    )
  }
