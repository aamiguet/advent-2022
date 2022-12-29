package ch.aamiguet.advent2022

final class Day19Test extends TestSuite:
  val d = Day19("input/test19.txt")

  test("day 19") {
    expect(
      d.solvePart1 === 33,
      //d.solvePart2 === 58
    )
  }
