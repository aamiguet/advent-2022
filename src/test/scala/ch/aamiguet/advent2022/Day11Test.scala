package ch.aamiguet.advent2022

final class Day11Test extends TestSuite:
  val d = Day11("input/test11.txt")

  test("day 11") {
    expect(
      d.solvePart1 === 10605,
      d.solvePart2 === 2713310158L,
    )
  }
