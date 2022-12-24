package ch.aamiguet.advent2022

final class Day20Test extends TestSuite:
  val d = Day20("input/test20.txt")

  test("day 20") {
    expect(
      d.solvePart1 === 3L,
      d.solvePart2 === 1623178306L,
    )
  }
