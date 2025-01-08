package ch.aamiguet.advent2022

final class Day23Test extends TestSuite:
  val d = Day23("input/test23.txt")

  test("day 23"):
    expect(
      d.solvePart1 === 110,
      d.solvePart2 === 20,
    )
