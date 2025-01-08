package ch.aamiguet.advent2022

final class Day19Test extends TestSuite:
  val d = Day19("input/test19.txt")

  test("day 19 - part 1"):
    expect(
      d.solvePart1 === 33,
    )

  test("day 19 - part 2"):
    expect(
      d.solvePart2 === 56 * 62,
    )
