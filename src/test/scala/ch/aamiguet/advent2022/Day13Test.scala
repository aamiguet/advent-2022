package ch.aamiguet.advent2022

final class Day13Test extends TestSuite:
  val d = Day13("input/test13.txt")

  test("day 13"):
    expect(
      d.solvePart1 === 13,
      d.solvePart2 === 140,
    )
