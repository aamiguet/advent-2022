package ch.aamiguet.advent2022

final class Day9Test extends TestSuite:
  val d = Day9("input/test9.txt")

  test("day 9") {
    expect(
      d.solvePart1 === 13,
      d.solvePart2 === 1,
    )
  }

  val d2 = Day9("input/test9-2.txt")

  test("day 9 - 2n input") {
    expect(
      d2.solvePart2 === 36,
    )
  }
