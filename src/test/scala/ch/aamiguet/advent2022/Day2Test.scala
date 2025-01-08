package ch.aamiguet.advent2022

final class Day2Test extends TestSuite:
  val day2 = Day2("input/test2.txt")

  test("day 2"):
    expect(
      day2.solvePart1 === 15,
      day2.solvePart2 === 12,
    )
