package ch.aamiguet.advent2022

final class Day4Test extends TestSuite:
  val d = Day4("input/test4.txt")

  test("day 4") {
    expect(
      d.solvePart1 === 2,
      d.solvePart2 === 4,
    )
  }
