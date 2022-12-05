package ch.aamiguet.advent2022

final class Day5Test extends TestSuite:
  val d = Day5("input/test5.txt")

  test("day 5") {
    expect(
      d.solvePart1 === "CMZ",
      d.solvePart2 === "MCD",
    )
  }
