package ch.aamiguet.advent2022

final class Day21Test extends TestSuite:
  val d = Day21("input/test21.txt")

  test("day 21"):
    expect(
      d.solvePart1 === 152,
      d.solvePart2 === 301,
    )
