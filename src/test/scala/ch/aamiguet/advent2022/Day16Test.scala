package ch.aamiguet.advent2022

final class Day16Test extends TestSuite:
  val d = Day16("input/test16.txt")

  test("day 16"):
    expect(
      d.solvePart1 === 1651,
      d.solvePart2 === 1707,
    )
