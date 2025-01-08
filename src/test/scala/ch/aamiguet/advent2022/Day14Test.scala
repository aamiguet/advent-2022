package ch.aamiguet.advent2022

final class Day14Test extends TestSuite:
  val d = Day14("input/test14.txt")

  test("day 14"):
    expect(
      d.solvePart1 === 24,
      d.solvePart2 === 93,
    )
