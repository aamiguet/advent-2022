package ch.aamiguet.advent2022

final class Day3Test extends TestSuite:
  val d = Day3("input/test3.txt")

  test("day 3"):
    expect(
      d.solvePart1 === 157,
      d.solvePart2 === 70,
    )
