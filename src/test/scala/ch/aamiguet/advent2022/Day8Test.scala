package ch.aamiguet.advent2022

final class Day8Test extends TestSuite:
  val d = Day8("input/test8.txt")

  test("day 8") {
    expect(
      d.solvePart1 === 21,
      d.solvePart2 === 8,
    )
  }
