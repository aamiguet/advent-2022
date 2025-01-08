package ch.aamiguet.advent2022

final class Day18Test extends TestSuite:
  val d = Day18("input/test18.txt")

  test("day 18"):
    expect(
      d.solvePart1 === 64,
      d.solvePart2 === 58
    )
