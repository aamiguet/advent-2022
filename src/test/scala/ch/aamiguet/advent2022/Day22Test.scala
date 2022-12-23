package ch.aamiguet.advent2022

final class Day22Test extends TestSuite:
  val d = Day22("input/test22.txt")

  test("day 22") {
    expect(
      d.solvePart1 === 6032,
      d.solvePart2 === 5031,
    )
  }
